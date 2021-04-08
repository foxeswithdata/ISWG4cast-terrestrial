library(sf)
library(tmap)
library(tidyverse)

sites = read.csv("FLUXNET data/sitelocations.csv") %>% 
  st_as_sf(coords = c(lat = "lon", lon = "lat"), crs = 4326)
tmap_mode("view")
tm_shape(sites[sites$network == "NEON",])+tm_dots(col = "blue", size = 0.25)+tm_shape(sites[sites$network == "FLUXNET",])+tm_dots(col = "red", size = 0.25)
