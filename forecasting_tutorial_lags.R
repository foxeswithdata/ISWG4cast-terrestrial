

library(lubridate)
# historical 
gee<-read.csv("GEE_Data.csv")
gee$Date<-as_date(gee$Date)
gee$Year<-year(gee$Date)
head(gee)
library(tidyr)
g<-gather(gee, "variable","value",7:17)
dim(g)
head(g)
gs<-aggregate(g$value,by=list(year=g$Year ,month=g$Month,siteID=g$siteID, variable=g$variable ), FUN=mean, na.rm=T )
s<-spread(gs, "variable","x")
head(s)

set.seed(20)
whitenoise <- ts(rnorm(273, mean=0.18))           
plot(whitenoise, main="White noise")
abline(h=0.18)


setwd("~/GitHub/forecasting-course/content/en/data")

data = read.csv('portal_timeseries.csv')
head(data)
NDVI.ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
NDVI.ts
plot(NDVI.ts, xlab = "Year", ylab="greenness", main="NDVI")
abline(h=0.18)

####################
summary(gee)
plot(gee$Date, gee$soil.moisture)
summary(s)


table( s$soil.moisture,s$ymo)
s$ymo<-paste(s$year, s$month)
bart.gpp<-ts(s[s$siteID=="BART","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
srer.gpp<-ts(s[s$siteID=="SRER","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
konz.gpp<-ts(s[s$siteID=="KONZ","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)
osbs.gpp<-ts(s[s$siteID=="OSBS","GPP"], start=c(2001,1), end = c(2020,12), frequency=12)

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
install.packages("forecast")
tsdisplay(NDVI.ts)


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




