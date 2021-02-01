library(tidyverse)
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(tidync)
library(stars)

# reading data function ####
noaa_gefs_read <- function(base_dir, date, cycle, sites){
  
  if(!(cycle %in% c("00","06","12","18"))){
    stop("cycle not available cycles of 00, 06, 12, 18")
  }
  
  cf_met_vars <- c("air_temperature")
  
  combined_met <- NULL
  for(k in 1:length(date)){
    for(i in 1:length(sites)){
      
      forecast_dir <- file.path(base_dir, sites[i], lubridate::as_date(date[k]),cycle)
      
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
        noaa_met$preddate = lubridate::as_date(date[k])
        
        combined_met <- rbind(combined_met, noaa_met)
        
      }
    }
    
  }
  return(combined_met)
}


# bart ####
# define settings
base_dir <- "./efi_neon_challenge/drivers/noaa/NOAAGEFS_1hr"
date <- c("2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01")
cycle <- "00"
sites <- "BART"

# extract dates from BART main folder
date_dir <- file.path(base_dir, sites)

date_folders <- list.files(date_dir, full.names = FALSE)

# let's try the tidync package
forecast_dir <- file.path(base_dir, sites, lubridate::as_date(date_folders[1]),cycle)


forecast_files <- list.files(forecast_dir, full.names = TRUE)


nfiles <-   length(forecast_files)


file = system.file(forecast_files[1])


tidync(forecast_files[6])
