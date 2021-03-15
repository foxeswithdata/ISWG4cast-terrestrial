# Script for generating metadata for the Ecological Forecasting Initiative- Terrestrial forecasting challenge; Team ISWG

library(EML)
library(ncdf4)
library(emld)
library(lubridate)
library(tibble)
library(dplyr)
library(tidyr)
library(reshape2)
emld::eml_version("eml-2.2.0")
set.seed(42)

# 1. load forecast dataset ####
dfs = read.csv("terrestrial-2021-01-01-ISWG.csv")

# 2. generate metadata ####
# define the included variables and attributes of the forecast
## likely only relevant for netCDF file submissions
attributes <- tibble::tribble(
  ~attributeName,     ~attributeDefinition,                          ~unit,                  ~formatString, ~numberType, ~definition,
  "time",              "[dimension]{time}",                          "year",                 "YYYY-MM-DD",  "numberType", NA,
  "statistic",          "[dimension]{type of statistic}",            "dimensionless",         NA,           "character",  NA,
  "siteID",             "[dimension]{neon site}",                    "dimensionless",         NA,           "character",  NA,
  "obs_flag",          "[dimension]{observation error}",             "dimensionless",         NA,           "integer",    NA,
  "nee",               "[variable]{net ecosystem exchange}",         "dimensionless",         NA,           "real",       NA,
  "le",                "[variable]{latent heat}",                    "dimensionless",         NA,           "real",       NA,
  "vswc",               "[variable]{volumetric soil water content}",  "dimensionless",         NA,           "real",       NA,
  "forecast",          "[flag]{whether time step assimilated data}", "dimensionless",         NA,           "integer",    NA,
  "data_assimilation", "[flag]{whether time step assimilated data}", "dimensionless",         NA,           "integer",    NA
) 
## note: EML uses a different unit standard than UDUNITS. For now use EML. EFI needs to provide a custom unitList.
attributes
attrList <- set_attributes(attributes, 
                           col_classes = c("Date", "character", "character", "numeric","numeric",
                                           "numeric","numeric", "numeric","numeric"))

# set metadata about the file itself (name, file type, size, MD5, etc)
physical <- set_physical("terrestrial-2021-01-01-ISWG.csv",
                         recordDelimiter='\n')
# set metadata for the file as a whole
dataTable <- eml$dataTable(
  entityName = "forecast",  ## this is a standard name
  entityDescription = "Forecast of NEE and LE for four NEON sites",
  physical = physical,
  attributeList = attrList)

# define author list
me <- list(
  list(individualName = list(givenName = "Alex", 
                             surName = "Young")),
  list(individualName = list(givenName = "Kathryn",
                             surName = "Fuller"),
       id = "https://orcid.org/0000-0001-9100-7635"),
  list(individualName = list(givenName = "Elisa",
                             surname = "Stefaniak")),
  list(individualName = list(givenName = "Jenna",
                             surName = "Zukswert")),
  list(individualName = list(givenName = "Laura",
                             surName = "Super"))
)

# define the geographic coverage of the forecast, using the provided geo.json file
fullgeographicCoverage <- jsonlite::read_json("meta/terrestrial_geo.json")
site_id_index <- NULL
for(i in 1:length(fullgeographicCoverage)){
  if(fullgeographicCoverage[[i]]$id %in% dfs$siteID)
    site_id_index <- c(site_id_index, i)
}
geographicCoverage <- fullgeographicCoverage[site_id_index]
start_time = min(dfs$time)
stop_time = max(dfs$time)
temporalCoverage <- list(rangeOfDates =
                           list(beginDate = list(calendarDate = start_time),
                                endDate = list(calendarDate = stop_time)))
#'Create the coverage EML
coverage <- list(geographicCoverage = geographicCoverage,
                 temporalCoverage = temporalCoverage)

# define keywords
keywordSet <- list(
  list(
    keywordThesaurus = "EFI controlled vocabulary",
    keyword = list("forecast",
                   "timeseries")
  ))

# attach abstract (optional)
# abstract_text <- system.file("extdata", "abstract.md", package="EFIstandards", mustWork = TRUE)

# combine metadata info
dataset = eml$dataset(
  title = "Summarized historical data as a forecast",
  creator = me,
  contact = "",
  pubDate = "2021-02-02",
  intellectualRights = "http://www.lternet.edu/data/netpolicy.html.",
  abstract =  "", ## insert abstract here if applicable
  dataTable = dataTable,
  keywordSet = keywordSet,
  coverage = coverage)

# define additional metadata, including error elements
additionalMetadata <- eml$additionalMetadata(
  metadata = list(
    forecast = list(
      ## Basic elements
      timestep = "1 day", ## should be udunits parsable; already in coverage -> temporalCoverage?
      forecast_horizon = "35 days",
      forecast_issue_time = "2021-02-02",
      forecast_iteration_id = "80808080",
      forecast_project_id = "ISWG",
      metadata_standard_version = "0.3",
      model_description = list(
        forecast_model_id = "80808080",
        name = "historical data summary as forecast",
        type = "statistical",
        repository = ""
      ),
      ## MODEL STRUCTURE & UNCERTAINTY CLASSES
      initial_conditions = list(
        # Possible values: absent, present, data_driven, propagates, assimilates
        status = "absent"
      ),
      drivers = list(
        status = "absent"
      ),
      parameters = list(
        status = "absent"
      ),
      random_effects = list(
        status = "absent"
      ),
      process_error = list(
        status = "propagates",
        propagates = list(
          type = "analytic"
        ),
        complexity = 1,   
        covariance = FALSE
      ),
      obs_error = list(
        status = "absent"
      )
    ) # forecast
  ) # metadata
) # eml$additionalMetadata

# generate eml
my_eml <- eml$eml(dataset = dataset,
                  additionalMetadata = additionalMetadata,
                  packageId = "80808080", 
                  system = "datetime"  ## system used to generate packageId
)

# 3. validate metadata ####
# eml_validate(my_eml)
## check that the EML is also a valid EFI forecast
# EFIstandards::forecast_validator(my_eml)
### did not function properly for January submission ###

# write eml to disk
write_eml(my_eml, "terrestrial_daily-forecast-2021-01-01-ISWG.xml")

# 4. submit metadata ####
aws.s3::put_object("terrestrial_daily-forecast-2021-01-01-ISWG.xml", bucket = "submissions")
