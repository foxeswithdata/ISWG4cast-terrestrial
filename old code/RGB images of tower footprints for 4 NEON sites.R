

library(neonUtilities)
library(rgeos)
library(raster)
library(geoNEON)

### SRER site
# tower coordinates
srer_tow<-c(515553.9, 3530547.12)
# download tile
byTileAOP("DP3.30010.001", site="SRER", year="2019", 
          check.size = F,buffer = 200,
          easting=srer_tow[1], northing=srer_tow[2] , savepath="neon_downloads")
# read in tower
img_srer<-stack("neon_downloads\\DP3.30010.001\\2019\\FullSite\\D14\\2019_SRER_3\\L3\\Camera\\Mosaic\\2019_SRER_3_515000_3530000_image.tif")
# plot tower
plotRGB(img_srer)
# inspect extent of image
extent(img_srer)
# make a 60 x 60 m box around the tower
srer_ext <- extent(srer_tow[1]-30,srer_tow[1]+30,srer_tow[2]-30,srer_tow[2]+30)
# crop tile from AOP to be 60 x 60 m tower footprint
tow_srer<-crop(img_srer,srer_ext)
# plot the tower
plotRGB(tow_srer)

### BART site
# tower coordinates
bart_tow<-c(316812.16, 4881511.55)
# download tile
byTileAOP("DP3.30010.001", site="BART", year="2019", 
          check.size = F,buffer = 100,
          easting=bart_tow[1], northing=bart_tow[2] , savepath="neon_downloads")
# read in tower
img_bart<-stack("neon_downloads\\DP3.30010.001\\2019\\FullSite\\D01\\2019_BART_5\\L3\\Camera\\Mosaic\\2019_BART_5_316000_4881000_image.tif")
# plot tower
plotRGB(img_bart)
# inspect extent of image
extent(img_bart)
# make a 60 x 60 m box around the tower
bart_ext <- extent(bart_tow[1]-30,bart_tow[1]+30,bart_tow[2]-30,bart_tow[2]+30)
# crop tile from AOP to be 60 x 60 m tower footprint
tow_bart<-crop(img_bart,bart_ext)
# plot the tower
plotRGB(tow_bart)

### OSBS site
# tower coordinates
osbs_tow<-c(403886.42, 3284767.49)
# download tile
byTileAOP("DP3.30010.001", site="OSBS", year="2019", 
          check.size = F,buffer = 100,
          easting=osbs_tow[1], northing=osbs_tow[2] , savepath="neon_downloads")
# read in tower
img_osbs<-stack("neon_downloads\\DP3.30010.001\\2019\\FullSite\\D03\\2019_OSBS_5\\L3\\Camera\\Mosaic\\2019_OSBS_5_403000_3284000_image.tif")
# plot tower
plotRGB(img_osbs)
# inspect extent of image
extent(img_osbs)
# make a 60 x 60 m box around the tower
osbs_ext <- extent(osbs_tow[1]-30,osbs_tow[1]+30,osbs_tow[2]-30,osbs_tow[2]+30)
# crop tile from AOP to be 60 x 60 m tower footprint
tow_osbs<-crop(img_osbs,osbs_ext)
# plot the tower
plotRGB(tow_osbs)

### KONZ site
# tower coordinates
konz_tow<-c(710729.8, 4330786.53)
# download tile
byTileAOP("DP3.30010.001", site="KONZ", year="2019", 
          check.size = F,buffer = 100,
          easting=konz_tow[1], northing=konz_tow[2] , savepath="neon_downloads")
# read in tower
img_konz<-stack("neon_downloads\\DP3.30010.001\\2019\\FullSite\\D06\\2019_KONZ_5\\L3\\Camera\\Mosaic\\2019_KONZ_5_710000_4330000_image.tif")
# plot tower
plotRGB(img_konz)
# inspect extent of image
extent(img_konz)
# make a 60 x 60 m box around the tower
konz_ext <- extent(konz_tow[1]-30,konz_tow[1]+30,konz_tow[2]-30,konz_tow[2]+30)
# crop tile from AOP to be 60 x 60 m tower footprint
tow_konz<-crop(img_konz,konz_ext)
# plot the tower
plotRGB(tow_konz)




# plot the 4 towers
par(mfrow=c(2,2))
plotRGB(tow_konz)
plotRGB(tow_bart)
plotRGB(tow_srer)
plotRGB(tow_osbs)





## ignore below-  For future mapping of soil moisture sensor location within each of the tower footprints
soil_moist <-
  loadByProduct(
    site = "SRER",
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE
  )

# you can get locations of veg plots conveniently using getLocTOS
## (except, use soil moisture no tree inventory
m <- getLocTOS(data = tree$vst_mappingandtagging,
               dataProd = "vst_mappingandtagging")





