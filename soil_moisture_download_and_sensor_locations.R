
## Alex uses neon_store to download soil moisture data


## Part 1- using neonUtilities.
## Part 2- using neon_store()

################################################################
library(neonUtilities)
library(lubridate)
library(ggplot2)
options(stringsAsFactors=F)


#####
# sd has Sensor Depths
sd<-read.csv("C:\\Users\\Dropcopter2\\Downloads\\SWC_depths.csv")
# site-plot-meas
sd$spd<-paste(sd$site, sd$plot, sd$measurementLevel)


# To load NEON data you need the neonUtilities package.  
# You choose the dates, sites, and product ID(s) you'd like
# This asks if you'd like to download, and gives the size of the download.

sm <- loadByProduct(dpID="DP1.00094.001", 
                    site=c("BART","SRER","KONZ","OSBS"),
                    startdate="2019-08", 
                    enddate="2019-08")

# look at the sm object
names(sm)    # You'll see 'slots' that you can write as dataframes

# Take the sensor_positions
spos<-sm$sensor_positions_00094

# this shows the unique names for each station (with 8 probes) for the 4 sites.
table(spos$siteID, spos$referenceName)

# average the repeated 8 lat and lon values to just get one lat lon for each station
posit<-aggregate(list(Lat=spos$referenceLatitude, Lon=spos$referenceLongitude), by=list(siteID=spos$siteID,refName=spos$referenceName), FUN="mean")

## When you plot the 4 sites, the multiple stations overlap.  But its cool!   Matches the locations of AZ, KS, FL, and NH from left to right.
plot(posit$Lon, posit$Lat, col=as.factor(posit$siteID), cex=4)

#subset just bart to see the sensors
bart.soil<-posit[posit$siteID=="BART",]
plot(bart.soil$Lon, bart.soil$Lat)


# Alex will pick up here to plot these sensor locations with a DEM and the tower location.

###############################################################################################



# Here work with half-hourly data for soil moisture
depth<-sm$SWS_30_minute
# turn into numeric for later matching with sensor depth measurements
depth$horizontalPosition<-as.numeric(depth$horizontalPosition)
depth$verticalPosition<-as.numeric(depth$verticalPosition)
# subtract 500 to get rid of the leading '50' in the vertical position codes.
depth$verticalPosition<-depth$verticalPosition-500
# subset to only use closest sensor
sensor1<-depth[depth$horizontalPosition=="1",]
# make a 'spd' site, plot, depth code to match in the sensor depths for any NEON site
sensor1$spd<-paste(sensor1$siteID, sensor1$horizontalPosition, sensor1$verticalPosition)

ggplot(sensor1, aes(x=startDateTime, y=VSWCMean, col=as.factor(verticalPosition)))+geom_point()+facet_wrap(~siteID)






################################################################

############ Below uses Neon_store
remotes::install_github("cboettig/neonstore")
library(neonstore)
# download soil moisture data
neonstore::neon_download(product = "DP1.00094.001", site = "OSBS", type = "basic")

## Soil moisture

#Load data
neon_store(table = "SWS_30_minute", n = 500) 
d2 <- neon_read(table = "sensor_positions") 
sm30 <- neon_table(table = "SWS_30_minute")
sensor_positions <- neon_table(table = "sensor_positions")

#Clean up sensor positions
#
sensor_positions <- sensor_positions %>% 
  mutate(horizontalPosition = str_sub(sensor_positions$HOR.VER, 1, 3),
         verticalPosition = str_sub(HOR.VER, 5, 7),
         siteID = str_sub(file, 10, 13)) %>% 
  rename(sensorDepths = zOffset) %>% 
  filter(siteID %in% c("KONZ", "BART", "OSBS", "SRER")) %>% 
  select(sensorDepths, horizontalPosition, verticalPosition, siteID)

sm30 <- left_join(sm30, sensor_positions, by = c("siteID", "verticalPosition", "horizontalPosition"))
```

Clean soil moisture with QC flag

```{r}
sm30 <- sm30 %>% 
  select(startDateTime, endDateTime, VSWCMean, siteID, horizontalPosition, verticalPosition, VSWCFinalQF, sensorDepths) %>% 
  mutate(VSWCMean = as.numeric(VSWCMean)) %>% 
  filter(VSWCFinalQF == 0)
```

### BART

Panels are depth

```{r}
sm30 %>% filter(siteID == "BART" & horizontalPosition == "001") %>% 
  ggplot(aes(x = startDateTime, y = VSWCMean)) +
  geom_point() +
  facet_wrap(~sensorDepths)
```







