
library(tidyr)
library(lubridate)
library(ggplot2)


grid<-read.csv("gee_data/bart_Gridmet_GEE.csv")

grid.bart<-read.csv("gee_data/bart_Gridmet_GEE.csv")
grid.bart$siteID<-"BART"
grid.konz<-read.csv("gee_data/konz_Gridmet_GEE.csv")
grid.konz$siteID<-"KONZ"
grid.srer<-read.csv("gee_data/srer_Gridmet_GEE.csv")
grid.srer$siteID<-"SRER"
grid.osbs<-read.csv("gee_data/osbs_Gridmet_GEE.csv")
grid.osbs$siteID<-"OSBS"


grid<-rbind(grid.bart, grid.konz, grid.srer, grid.osbs)
gr<-gather(grid, "variable","value",2:10)
head(gr)

GPP<-read.csv("gee_data/GPP_GEE_iswg.csv")
GPP$variable<-"GPP"
gpp<-gather(GPP, "siteID","value",2:5)
head(gpp)
tail(gpp)
gpp[3000:3010,]

gpp<-gpp[ ,c(1,3,2,4)]

smap<-read.csv("gee_data/soil_moisture_GEE_iswg.csv")
smap$variable<-"soil.moisture"
sm<-gather(smap, "siteID","value",2:5)
sm<-sm[,c(1,3,2,4)]
head(sm)

iswg<-rbind(gr, sm, gpp)

iswg$Date<-mdy(iswg$system.time_start)
iswg$Day<-yday(iswg$Date)
iswg$Month<-month(iswg$Date)
iswg$value<-as.numeric(iswg$value)
str(iswg)


s<-spread(iswg, variable, value)

head(s)
library(ggplot2)
dev.off()
str(s)
table(s$etr)

write.csv(s, file="GEE_data.csv")


ggplot(iswg[iswg$variable=="smap",], aes(x=Date, y=value, col=siteID))+geom_point()

ggplot(iswg[iswg$variable=="GPP",], aes(x=Date, y=value, col=siteID))+geom_point()

ggplot(iswg[iswg$variable=="pr",], aes(x=Date, y=value, col=siteID))+geom_point()


ggplot(s, aes(x=Date, y=GPP, col=siteID))+geom_point()
ggplot(s, aes(x=Date, y=soil.moisture, col=siteID))+geom_point()


ggplot(s, aes(x=soil.moisture, y=GPP, col=siteID))+geom_point()+geom_smooth(method="lm")
ggplot(s, aes(x=rmax, y=etr, col=siteID))+geom_point()+geom_smooth(method="lm")
ggplot(s, aes(x=srad, y=GPP, col=siteID))+geom_point()+geom_smooth(method="lm")


