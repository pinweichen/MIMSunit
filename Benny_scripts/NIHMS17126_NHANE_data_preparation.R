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
  install.packages("/Volumes/chps_digital_health_core_general/Benny_Actigraphy/NHANES/srcs/packages/MIMSunit/", repos = NULL, type="source")
  
  library(MIMSunit)
} else {
  library(MIMSunit)
}
rm(list=ls()) 
options(digits.secs = 3)

# Path ----
OS_p <- "/Volumes"
#OS_p <- "/mnt/isilon/"
# NHANES path
NHANES_p <- file.path(OS_p, "chps_digital_health_core_general/Benny_Actigraphy/NHANES/")

# number of NHANES folders
nhanes_folders <- list.files(NHANES_p, pattern = "NHANES_20")

# NHANES folders
years_list <- word(nhanes_folders,2, sep = "_")

# Years used


for(nhanes_list_n in 1:length(nhanes_folders)) {
  year_name <- years_list[nhanes_list_n]
  if (year_name %in% "2013-2014"){
    symbol_nm <- "H"
  } else if(year_name %in% "2011-2012"){
    symbol_nm <- "G"
  } else {
    print("Incorrect year range")
  }
  
  demo <-   haven::read_xpt(paste0(NHANES_p,nhanes_folders[nhanes_list_n],"/DEMO_",symbol_nm,".XPT")) 
  # paymin <- haven::read_xpt(paste0(NHANES_p,nhanes_folders[nhanes_list_n],".PAXMIN_",symbol_nm,".XPT"))
  payday <- haven::read_xpt(paste0(NHANES_p,nhanes_folders[nhanes_list_n],"/PAXDAY_",symbol_nm,".XPT")) 
  browser()
  
}


setDT(paymin)
dynamic_range = c(-8, 8)
epoch_length = '60 sec'




raw_data_extract <- paste0(general, "MAP_test_extracted/")
mims_processed_results_p <- paste0(general,"/MIMS_processed_results/") #last output from MIMS


