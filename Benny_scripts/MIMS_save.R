# MIMS unit NHANE 2011-2012 testing
# Compare the NHANE by minute data 
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

general <- "/mnt/isilon//chps_digital_health_core_general/Benny_Actigraphy/NHANES/NHANES_2011-2012/"
#general <- "/Volumes/chps_digital_health_core_general/Benny_Actigraphy/NHANES/NHANES_2011-2012/"
raw_data_extract <- paste0(general, "MAP_test_extracted/")
mims_processed_results_p <- paste0(general,"/MIMS_processed_results/") #last output from MIMS
demo<-haven::read_xpt(paste0(general,"DEMO_G.XPT")) 
paymin<-haven::read_xpt(paste0(general,"PAXMIN_G.XPT")) 
setDT(paymin)
dynamic_range = c(-8, 8)
epoch_length = '60 sec'

ls_file_folders <- list.files(mims_processed_results_p)

for (sub_n in 1:length(ls_file_folders)){
  sub_name <- ls_file_folders[sub_n]
  mims_sub_folder <- paste0(mims_processed_results_p,"/",sub_name,"/")
  csv_sub <- fread(paste0(mims_sub_folder,"MIMS_processed_1_min.csv"))
  paymin_sub <- paymin[SEQN %in% sub_name,]
  csv_sub[,dif_mims_unit := PAXMTSM]
}




