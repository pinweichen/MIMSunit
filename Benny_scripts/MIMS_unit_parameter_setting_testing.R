library(MIMSunit)
library(devtools)
library(lubridate)
library(stringr)
library(GGIR)
library(GGIRp1)
library(data.table)
library(tictoc)
library(dplyr)
library(read.gt3x)
library(ggplot2)
rm(list=ls()) 
options(digits.secs = 3)

brand = "actigraph"

ls_gt3x_csv <- list.files(datadir)

if (brand == "geneactiv" |brand == "actigraph" ){
  dynamic_range = c(-8, 8)
  
}


# output_path = "/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/WS-Pipeline-for-CHPS/data/result/GGIRp1_gt3x_test/"
# testing_name = "/gt3x_readmyacc_test/" # <--------- change
# outputdir = paste0(output_path,testing_name)
# dir.create(outputdir,recursive = TRUE)
# =============================================

# install.packages("/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/MIMSunit
# "                 , repos = NULL, type="source")
datadir = "/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/WS-Pipeline-for-CHPS/data/gt3x_csv_new_format_fix_milliseconds/"

#read.myacc.csv function
setwd("/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/WS-Pipeline-for-CHPS/code/R")

# load timezone legend
timezone_legend <- fread(paste0(getwd(), "/Supplementary_data/timezone_legend.csv")) 

# Change this argument
algo_name = "ColeKripke1992" # The argument can be "ColeKripke1992", "Sadeh1994", "vanHees2015" 

# Change this argument
Sadeh_axis = "Y" # This parameter needs to be "Y" if the algo_name is "ColeKripke1992" or "Sadeh1994"
desiredtz = "EST"
# Load gt3x files
datadir_gt3x <- "/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/WS-Pipeline-for-CHPS/data/gt3x/"
filelist_gt3x = isfilelist(datadir_gt3x)
# list all accelerometer files
dir2fn_gt3x = GGIRp1::datadir2fnames(datadir_gt3x, filelist_gt3x)
fnames_gt3x = dir2fn_gt3x$fnames
fnamesfull_gt3x = dir2fn_gt3x$fnamesfull
datafile_gt3x = as.character(fnamesfull_gt3x[3])
dt_gt3x <- as.data.frame(read.gt3x::read.gt3x(path = datafile_gt3x, asDataFrame = TRUE))

setDT(dt_gt3x)
# browser()
# Update timezone based on the gt3x header file.
if(length(attributes(dt_gt3x)$header$TimeZone) > 0) {
  ts_utc <- as.numeric(word(attributes(dt_gt3x)$header$TimeZone, 1 , sep = ":"))
  timezone <- dplyr::recode(
    ts_utc, 
    !!!setNames( timezone_legend$timezone, as.numeric(timezone_legend$utc_offset))
  )
} 

dt_gt3x$time <- force_tz(dt_gt3x$time,tzone = timezone)

filelist = isfilelist(datadir)

# list all accelerometer files
dir2fn = GGIR::datadir2fnames(datadir, filelist)
fnames = dir2fn$fnames
fnamesfull = dir2fn$fnamesfull
datafile = as.character(fnamesfull[3])
# Read with fread
dt_std <- fread(datafile,skip =9)
dt_std <- dt_std[,1:4]
colnames(dt_std) <- c("Time","x","y","z")
dt_std$Time <- as.POSIXct(dt_std$Time, tz = timezone)

epoch_length = '60 sec'
colnames(dt_std)[1] <- "HEADER_TIME_STAMP"
mims_data <- mims_unit(dt_std, epoch = '60 sec', dynamic_range=dynamic_range, use_snapshot_to_check= TRUE)

setDT(mims_data)

ggplot() +
  geom_point(data = dt_std[HEADER_TIME_STAMP < as.POSIXct("2023-09-08 12:00:00"),], aes( x = HEADER_TIME_STAMP, y = x)) + 
  geom_point(data = mims_data[HEADER_TIME_STAMP < as.POSIXct("2023-09-08 12:00:00"),], aes( x = HEADER_TIME_STAMP, y = MIMS_UNIT), color = "red", alpha = 0.4)

