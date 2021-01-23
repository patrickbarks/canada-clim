
## requires
library(tidyverse)
library(curl)


## create raw data dir
dir_data_raw <- file.path("dat-raw", Sys.Date())
if (!dir.exists(dir_data_raw)) dir.create(dir_data_raw, recursive = TRUE)


## main ftp dir
ftp_dir <- "ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/"


## check files in ftp directory
ftp_con <- curl::curl(url = ftp_dir)
# use arg handle = curl::new_handle(dirlistonly = TRUE) to only list files
ftp_files <- readLines(ftp_con)
close(ftp_con)
ftp_files


## Relevant files (as of 2021-01-20)
# - files '_Gen2' reflect 2nd-gen estimates for 339 stations (Vincent et al. 2012)
# - files with no suffix reflect the 3rd-gen estimates for 508 stations (Vincent et al. 2020)
file_temperature <- "Homog_monthly_mean_temp.zip"
file_stations <- "Temperature_Stations.xls"


## download temperature data
ftp_path <- file.path(ftp_dir, file_temperature)

dest_dir <- tempdir()
dest_file <- file.path(dest_dir, file_temperature)

# download
curl::curl_download(ftp_path, dest_file)

# unzip into data-raw/{date}/{group}
file_group <- gsub("\\..*$", "", file_temperature)
dir_data_raw_group <- file.path(dir_data_raw, file_group)
if (!dir.exists(dir_data_raw_group)) dir.create(dir_data_raw_group)

unzip(dest_file, exdir = dir_data_raw)


## download stations metadata
ftp_path_stations <- file.path(ftp_dir, file_stations)
dest_file_stations <- file.path(dir_data_raw, stations_name)
curl::curl_download(ftp_stations, dest_file_stations)

