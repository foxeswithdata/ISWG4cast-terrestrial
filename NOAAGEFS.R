#' Reading NOAA GEFS Forecasts from netcdf
#'
#' @param base_dir directory of the particular NOAA GEFS model (i.e., NOAAGEFS_1hr)
#' @param date date in YYYY-MM-DD of the forecast
#' @param cycle cycle of the forecast; the options are 00, 06, 12, 18
#' @param sites list if siteID
#'
#' @return data frame
#' @export
#'
#' @examples
#' 
#' library(tidyverse)
#' base_dir <- "/efi_neon_challenge/drivers/noaa/NOAAGEFS_1hr"
#' date <- "2020-10-21"
#' cycle <- "00"
#' sites <- c("ABBY", "BART")
#' noaa_gefs_read(base_dir, date, cycle, sites)
#' 
#' 

library(tidyverse)
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(tidync)
library(stars)

# reading data function ####
noaa_gefs_read <- function(base_dir, date, cycle, sites){
  
  if(!(cycle %in% c("00","06","12","18"))){
    stop("cycle not available cycles of 00, 06,12,18")
  }
  
  cf_met_vars <- c("air_temperature")
  
  combined_met <- NULL
  
  for(i in 1:length(sites)){
    
    forecast_dir <- file.path(base_dir, sites[i], lubridate::as_date(date),cycle)
    
    forecast_files <- list.files(forecast_dir, full.names = TRUE)
    
    nfiles <-   length(forecast_files)
    
    for(j in 1:nfiles){
      
      ens <- dplyr::last(unlist(stringr::str_split(basename(forecast_files[j]),"_")))
      ens <- stringr::str_sub(ens,1,5)
      noaa_met_nc <- ncdf4::nc_open(forecast_files[j], verbose = TRUE)
      noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
      origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
      origin <- lubridate::ymd_hm(origin)
      noaa_met_time <- origin + lubridate::hours(noaa_met_time)
      noaa_met <- tibble::tibble(time = noaa_met_time)
      
      for(v in 1:length(cf_met_vars)){
        noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[v]))
      }
      
      ncdf4::nc_close(noaa_met_nc)
      
      names(noaa_met) <- c("time", cf_met_vars)
      
      noaa_met <- noaa_met %>% 
        dplyr::mutate(siteID = sites[i],
                      ensemble = as.numeric(stringr::str_sub(ens,4,5))) %>% 
        dplyr::select("siteID","ensemble","time",all_of(cf_met_vars))
      
      combined_met <- rbind(combined_met, noaa_met)
      
    }
  }
  return(combined_met)
}


# run function

base_dir <- "./efi_neon_challenge/drivers/noaa/NOAAGEFS_1hr"

date <- "2020-09-25"

cycle <- "00"

sites <- "BART"

bart = noaa_gefs_read(base_dir, date, cycle, sites)

# extract dates from BART main folder
date_dir <- file.path(base_dir, sites)
date_folders <- list.files(date_dir, full.names = FALSE)

# loop through dates and extract files
times = seq(as.POSIXct("2020-09-25"), as.POSIXct("2021-01-17"), by = difftime("2020-09-25 01:00:00", "2020-09-25 00:00:00", units = "hours"))
new = data.frame(datetime = times)
new$availability = 0

for(i in 1:length(date_folders)){
  x = noaa_gefs_read(base_dir, date_folders[i], cycle, sites)
  new2 = x %>% 
    group_by(time) %>% 
    tally() 
  new$availability[new$datetime %in% new2$time] = new$availability + new2$time
}
# stops working, I think on 2020-10-21. Removing this date doesn't help

# let's try the tidync package
forecast_dir <- file.path(base_dir, sites, lubridate::as_date(date_folders[1]),cycle)

forecast_files <- list.files(forecast_dir, full.names = TRUE)

nfiles <-   length(forecast_files)

file = system.file(forecast_files[1])

tidync(forecast_files[1])
# this works

read_ncdf(forecast_files[6])
# this works

## what is next?
# we want just the NOAA predictions at the beginning of each month
# 1) grab just the predictions from the first day of the month, 00 cycle
# 2) throw out the first ensemble
# 3) calculate the mean and standard error of all ensembles
