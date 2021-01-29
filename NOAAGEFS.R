# script for reading, summarising and exporting NOAA GEFS netcdf files to CSV
# 1. data read function
#     - NOAA GEFS data were taken from: https://data.ecoforecast.org/minio/drivers/
#     - function for loading netCDF files was modified from https://github.com/eco4cast/neon4cast-shared-utilities/blob/main/noaa_gefs_read.R
# 2. examine data and export plot
# 3. compile data for all sites and export to CSV

library(tidyverse)
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(tidync)
library(stars)

# 1. ####
noaa_gefs_read <- function(base_dir, date, cycle, sites){
  
  if(!(cycle %in% c("00","06","12","18"))){
    stop("cycle not available cycles of 00, 06, 12, 18")
  }
  
  cf_met_vars <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "specific_humidity", "cloud_area_fraction", "wind_speed")
  
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


# bart air temp plotting ####
# define settings
base_dir <- "./efi_neon_challenge/drivers/noaa/NOAAGEFS_1hr"
date <- c("2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01")
cycle <- "00"
sites <- "BART"

# compile files and remove the "00" ensemble, which does not predict the entire month
df = noaa_gefs_read(base_dir, date, cycle, sites) %>% 
  dplyr::filter(ensemble != "0")

# generate statistics


df_stats = aggregate(data = df, air_temperature ~ siteID + time + preddate, FUN = function(x) c(avg = mean(x-273.15),upper = mean(x-273.15) + sd(x-273.15)/sqrt(length(x-273.15)), lower = mean(x-273.15) - sd(x-273.15)/sqrt(length(x-273.15))), simplify = TRUE, drop = TRUE)

# reformat for convenience
val<-data.frame(df_stats[["air_temperature"]])
df_stats$airT_mean<-val$avg
df_stats$airT_upperconf<-val$upper
df_stats$airT_lowerconf<-val$lower

str(df_stats)

# remove original variables
df_stats = df_stats %>% 
  dplyr::select(-siteID,
                -air_temperature)

# remove extraneous objects
rm(df, val)

# split data frame into list object to plot predictions based on start date
df_stats$preddate = as.factor(df_stats$preddate)
split_df<-split(df_stats, droplevels(df_stats$preddate))

# generate standard axes for plotting
ymin = min(df_stats$airT_lowerconf)
ymax = max(df_stats$airT_upperconf)
xmin = min(df_stats$time)
xmax = max(df_stats$time)

# plot
#tiff(file = paste(sites[1], "airtemp.tif", sep = ""), width =1600, height = 1200, units = "px", res = 200)
colours = rainbow(length(split_df))
with(split_df[[1]], plot(airT_mean ~ time, 
                     type = "l",
                     bty ='l',
                     ylim = c(ymin, ymax),
                     xlim = c(xmin, xmax),
                     ylab = expression(paste("Air Temperature (",degree~C,")")),
                     xlab = "Date",
                     col = colours[1]))
with(split_df[[1]], polygon(c(time, rev(time)), c(airT_lowerconf, rev(airT_upperconf)), col =  "gray70", border = NA))
with(split_df[[1]], points(airT_mean ~ time, type = "l", col = colours[1], lwd = 0.75))
for(i in (c(2:length(split_df)))){
  with(split_df[[i]], polygon(c(time, rev(time)), c(airT_lowerconf, rev(airT_upperconf)), col = "gray70", border = NA))
  with(split_df[[i]], points(airT_mean ~ time, type = "l", col = colours[i], lwd = 0.75))
}
dev.off()

# load and export dataset ####
# define settings
base_dir <- "./efi_neon_challenge/drivers/noaa/NOAAGEFS_1hr"
date <- "2021-01-01"
cycle <- "00"
sites <- c("BART", "KONZ", "OSBS", "SRER")

# load data and format for export
df = noaa_gefs_read(base_dir, date, cycle, sites) %>% 
  dplyr::filter(ensemble != "0") %>% 
  pivot_longer(cols = c(air_temperature, air_pressure, relative_humidity, surface_downwelling_longwave_flux_in_air, surface_downwelling_shortwave_flux_in_air, precipitation_flux, specific_humidity, cloud_area_fraction, wind_speed), names_to = "vars")

# change temperature units from kelvin to celsius
df[df$vars == "air_temperature",]$value = df[df$vars == "air_temperature",]$value - 273.15

# change names
names(df) = c("siteID", "ensemble", "forecast_time", "forecast_startdate", "variable", "value")

write.csv(df, paste("NOAA_GEFS_CSVs/", date, "_allvariables.csv", sep = ""))


#####

# Alex makes a quick ggplot for the January forcast

library(ggplot2)

str(df)

ggplot(df, aes(x=forecast_time, y=value, col=siteID))+geom_point()+facet_grid(variable~siteID, scales="free_y")


