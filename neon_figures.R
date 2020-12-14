library(neonUtilities)
library(neonstore)

library(tidyverse)
library(lubridate)
library(contentid)

# Terrestrial
#DP4.00200.001 & DP1.00094.001
neon_download("DP4.00200.001")

neon_store(product = "DP4.00200.001", n = 500) 
flux_data <- neon_table(table = "nsae-basic")

flux_data <- flux_data %>% 
  mutate(time = as_datetime(timeBgn))

co2_data <- flux_data %>% 
  filter(qfqm.fluxCo2.turb.qfFinl == 0) %>% 
  select(time,data.fluxCo2.turb.flux, siteID) %>% 
  rename(nee = data.fluxCo2.turb.flux)


le_data <-  flux_data %>% 
  filter(qfqm.fluxH2o.turb.qfFinl == 0) %>% 
  select(time,data.fluxH2o.turb.flux, siteID)%>% 
  rename(le = data.fluxH2o.turb.flux)

earliest <- min(as_datetime(c(co2_data$time,le_data$time)), na.rm = TRUE)
latest <- max(as_datetime(c(co2_data$time,le_data$time)), na.rm = TRUE)


full_time <- seq(min(c(co2_data$time,le_data$time), na.rm = TRUE), 
                 max(c(co2_data$time,le_data$time), na.rm = TRUE), 
                 by = "30 min")

full_time <- tibble(time = rep(full_time, 4),
                    siteID = c(rep("BART", length(full_time)),
                               rep("KONZ", length(full_time)),
                               rep("OSBS", length(full_time)),
                               rep("SRER", length(full_time))))


flux_target1 <- left_join(full_time, co2_data, by = c("time", "siteID"))
flux_target_30m <- left_join(flux_target1, le_data, by = c("time", "siteID"))

valid_dates <- flux_target_30m %>% 
  mutate(date = as_date(time)) %>% 
  filter(!is.na(nee) & !is.na(le)) %>% 
  group_by(date, siteID) %>% 
  summarise(count = n()) %>% 
  filter(count >= 44)

flux_target_daily <- flux_target_30m %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date, siteID) %>% 
  summarize(nee = mean(nee, na.rm = TRUE),
            le = mean(le, na.rm = TRUE)) %>% 
  mutate(nee = ifelse(date %in% valid_dates$date, nee, NA),
         le = ifelse(date %in% valid_dates$date, le, NA)) %>% 
  rename(time = date) %>% 
  mutate(nee = (nee * 12 / 1000000) * (60 * 60 * 24))