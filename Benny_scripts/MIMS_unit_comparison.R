# MIMS unit NHANE 2011-2012 testing
# This test contains 320 subjects
if(!require(tidyverse)){
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  library(tidyverse)
} else {
  library(tidyverse)
}

if(!require(devtools)){
  install.packages("devtools", repos = "http://cran.us.r-project.org")
  library(devtools)
} else {
  library(devtools)
}

if(!require(data.table)){
  install.packages("data.table", repos = "http://cran.us.r-project.org")
  library(data.table)
} else {
  library(data.table)
}
if(!require(MIMSunit)){
  install.packages("/mnt/isilon//chps_digital_health_core_general/Benny_Actigraphy/NHANES/srcs/packages/MIMSunit/", repos = NULL, type="source")
  
  library(MIMSunit)
} else {
  library(MIMSunit)
}

rm(list=ls()) 
options(digits.secs = 3)
# year_range <- "2011-2012"
year_range <- "2013-2014"

if (year_range %in% "2013-2014"){
  symbol_nm <- "H"
} else if(years_range %in% "2011-2012"){
  symbol_nm <- "G"
} else {
  print("Incorrect year range")
}


general <- paste0("/mnt/isilon//chps_digital_health_core_general/Benny_Actigraphy/NHANES/NHANES_",year_range,"/")
#general <- "/Volumes/chps_digital_health_core_general/Benny_Actigraphy/NHANES/NHANES_2011-2012/"
raw_data_extract <- paste0(general, "MAP_test_extracted/")
# 
# demo<-haven::read_xpt(paste0(general,"DEMO_",symbol_nm,".XPT")) 
paymin<-haven::read_xpt(paste0(general,"PAXMIN_",symbol_nm,".XPT")) 
setDT(paymin)
dynamic_range = c(-8, 8)
epoch_length = '60 sec'

ls_file_folders <- list.files(paste0(general,"/MIMS_processed_results/"))

ls_error_csv <- list()
ls_csv_stat <- list()
for (file_n in 1:length(ls_file_folders)){
  file_name <- ls_file_folders[file_n]
  mims_processed_results_p <- paste0(general,"/MIMS_processed_results/",file_name,"/")
  csv_all <- fread(paste0(mims_processed_results_p,"MIMS_processed_1_min.csv"))
  setDT(csv_all)
  paymin_sub <- paymin[SEQN %in% file_name,]
  csv_all <- cbind(csv_all,paymin_sub[,.(PAXMTSM)])
  csv_all[, MIMS_UNIT := round(MIMS_UNIT, digits = 3)]
  csv_all[,diff_mims := PAXMTSM - MIMS_UNIT]
  csv_all[, ID := file_name]
  csv_stat <- csv_all[,.(ID = file_name, 
                         average_diff = mean(diff_mims),
                         sd_diff = sd(diff_mims),
                         min_diff = min(diff_mims),
                         max_diff = max(diff_mims),
                         count_zero = sum(diff_mims==0),
                         count_all =.N)]
  error_csv <- csv_all[diff_mims <= (csv_stat$average_diff - 1*csv_stat$sd_diff) |
                         diff_mims >=(csv_stat$average_diff + 1*csv_stat$sd_diff),]
  ls_error_csv[[file_n]] <- error_csv
  ls_csv_stat[[file_n]] <- csv_stat
  
}

error_csv_all <- rbindlist(ls_error_csv)
csv_stat_all <- rbindlist(ls_csv_stat)


error_report_path <- paste0(general,"/error_report/")
dir.create(error_report_path,recursive = T)
fwrite(error_csv_all, paste0(error_report_path,"error_list.csv"))
fwrite(csv_stat_all, paste0(error_report_path,"all_data_stat.csv"))

library(ggplot2)
ggplot()+ 
  geom_histogram(data = csv_stat_all, aes(x = count_zero))

ggplot() +
  geom_boxplot(data = csv_stat_all, aes(x = average_diff))



