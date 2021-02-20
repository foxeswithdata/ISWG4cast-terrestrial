
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
str(iswg)


s<-spread(iswg, variable, value)

head(s)
library(ggplot2)
dev.off()
ggplot(s, aes(x=Date, y=etr, col=siteID))+geom_point()+facet_wrap(~variable)









