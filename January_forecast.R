

library(tidyverse)
library(lubridate)
#library(rjags)
#library(tidybayes)
library(modelr)
library(aws.s3)
library(prov)
library(EFIstandards)
library(EML)
library(jsonlite)

## trying to upload a .csv file with the metadata documentation.
  # here is the .csv file I'd like to upload
janfx<-read.csv("terrestrial-2021-01-01-ISWG.csv")
janfx$time<-ymd(janfx$time)
janfx<-janfx[,-1]
str(janfx)



#'Is the forecast run on the Ecological Forecasting Initiative Server?
#'Setting to TRUE published the forecast on the server.
efi_server <- TRUE

#' List of team members. Used in the generation of the metadata
team_list <- list(list(individualName = list(givenName = "Alex",  surName ="Young")),
                  list(individualName = list(givenName = "Kathryn",  surName ="Fuller")))

#'Team name code
team_name <- "ISWG"


# set the start of forecast???  IDK why the max date is 2021_01_02. 
max_time <- max(janfx$time) - days(1)
max_time

# start forecast should be jan 1?
start_forecast <- max_time


#'#Generate metadata
#'Get system time for setting the issue time of the forecast
curr_time <- with_tz(Sys.time(), "UTC")
#forecast_issue_time <- format(curr_time,format = "%Y-%m-%d %H:%M:%SZ", usetz = F)
forecast_issue_time <- as_date(curr_time)
forecast_iteration_id <- start_forecast

# is this right?
forecast_iteration_id


#' The team name is the `forecast_model_id`
forecast_model_id <- team_name

source("generate_metadata.R")


#'Save file as CSV in the
#'[theme_name]-[year]-[month]-[date]-[team_name].csv
forecast_file_name_base <- paste0("terrestrial_daily-",as_date(start_forecast),"-",team_name)


forecast_file <- paste0(forecast_file_name_base, ".csv.gz")
forecast_file
write_csv(janfx, forecast_file)




meta_data_filename <- generate_metadata(forecast_file =  forecast_file,
                                        metadata_yaml = "metadata.yml",
                                        forecast_issue_time = as_date(with_tz(Sys.time(), "UTC")),
                                        forecast_iteration_id = start_forecast,
                                        forecast_file_name_base = forecast_file_name_base)


#'Publish the forecast automatically.  Run only on EFI Challenge server
if(efi_server){
  source("../neon4cast-shared-utilities/publish.R")
  publish(code = "January_forecast.R",
          data_in = c("terrestrial_daily-targets.csv.gz", "terrestrial_30min-targets.csv.gz"),
          data_out = forecast_file,
          meta = meta_data_filename,
          prefix = "terrestrial/",
          bucket = "forecasts")
}



