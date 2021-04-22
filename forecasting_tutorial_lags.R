
library(tidyr)
library(lubridate)
# historical 
gee<-read.csv("GEE_Data.csv")
gee$Date<-as_date(gee$Date)
gee$Year<-year(gee$Date)
gee$week<-week(gee$Date)
head(gee)


# make a wide spreadsheet long by using 'gather'
g<-gather(gee, "variable","value",7:17)
dim(g)
head(g)

# this gets a weekly average
gs<-aggregate(g$value,by=list(year=g$Year ,month=g$Month,week=g$week,siteID=g$siteID, variable=g$variable ), FUN=mean, na.rm=T )
head(gs)
s<-spread(gs, "variable","x")
head(s)




library(ggplot2)
ggplot(gs[gs$year>2015,], aes(x=week, y=x, col=year))+geom_point()+
  facet_wrap(~variable, scales="free")

## To get a weekly average, average by week
w<-s[s$year>2015,]

## make a time series of the data with weeks?
table(gs$week, gs$year)

bart.gpp<-ts(s[s$siteID=="BART","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)

## If you want to follow the tutorial online, this is the github directory
setwd("~/GitHub/forecasting-course/content/en/data")


data = read.csv('portal_timeseries.csv')
head(data)

set.seed(20)
whitenoise <- ts(rnorm(273, mean=0.18))           
plot(whitenoise, main="White noise")
abline(h=0.18)

NDVI.ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
NDVI.ts
plot(NDVI.ts, xlab = "Year", ylab="greenness", main="NDVI")
abline(h=0.18)


## Otherwise, I use the gee dataset below to go through the tutorial
summary(gee)
plot(gee$Date, gee$soil.moisture)
summary(s)


table( s$soil.moisture,s$ymo)
s$ymo<-paste(s$year, s$month)
bart.gpp<-ts(s[s$siteID=="BART","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
srer.gpp<-ts(s[s$siteID=="SRER","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
konz.gpp<-ts(s[s$siteID=="KONZ","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
osbs.gpp<-ts(s[s$siteID=="OSBS","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)


# look at evapotransporation
etr.osbs<-na.omit(s[s$siteID=="OSBS",])
osbs.etr<-ts(etr.osbs[ ,"etr"], start=c(2016,1), end=c(2020,12),frequency=12)



par(mfrow=c(2,2))
plot(bart.gpp, xlab="Year",ylab="GPP", main="Bartlett EF")
plot(srer.gpp, xlab="Year",ylab="GPP", main="Santa rita EF")
plot(osbs.gpp, xlab="Year",ylab="GPP", main="Ordway swisher bio station")
plot(konz.gpp, xlab="Year",ylab="GPP", main="Konza prairie")


lag.plot(bart.gpp, lags=12, do.lines=FALSE)

par(mfrow=c(2,2))
acf(bart.gpp)
acf(srer.gpp)
acf(konz.gpp)
acf(osbs.gpp)

 # note pacf starts at lag1 not 0 so this can confuse people
par(mfrow=c(2,2))
pacf(bart.gpp)
pacf(srer.gpp)
pacf(konz.gpp)
pacf(osbs.gpp)


library(forecast)
#install.packages("forecast")
par(mfrow=c(2,2))
tsdisplay(osbs.gpp)
tsdisplay(srer.gpp)

tsdisplay(osbs.etr)

## soil moisture
summary(s[is.na(s$soil.moisture),])
bart.smap<-ts(s[s$siteID=="BART","soil.moisture"], start=c(2001,1), end = c(2020,12), frequency=12)
bart.smap
srer.smap<-ts(s[s$siteID=="SRER","soil.moisture"], start=c(2001,1), end = c(2020,12), frequency=12)
konz.smap<-ts(s[s$siteID=="KONZ","soil.moisture"], start=c(2001,1), end = c(2020,12), frequency=12)
osbs.smap<-ts(s[s$siteID=="OSBS","soil.moisture"], start=c(2001,1), end = c(2020,12), frequency=12)


par(mfrow=c(2,2))
plot(bart.smap, xlab="Year",ylab="smap", main="Bartlett EF")
plot(srer.smap, xlab="Year",ylab="smap", main="Santa rita EF")
plot(osbs.smap, xlab="Year",ylab="smap", main="Ordway swisher bio station")
plot(konz.smap, xlab="Year",ylab="smap", main="Konza prairie")












