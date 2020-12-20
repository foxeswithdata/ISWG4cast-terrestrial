
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(lubridate)
library(contentid)
library(data.table)

# Notes on NEONstore
#  neonstore the library can be found here: remotes::install_github("cboettig/neonstore")

ind<-as.data.frame(neon_index())  # if you have anything downloaded, this will tell you what and at which sites
table(ind$product, ind$site)


# Thanks to EFI, the target variables that have been passed through a QA filter are available here. 
#The NEE, LE, and Soil moisture are contained with this dataet. 
fd<-fread("terrestrial_30min-targets.csv.gz")
head(fd)
#fdaily<-fread("terrestrial_daily-targets.csv.gz")



#This downloads the PAR (from the tower)
neon_download(product = "DP1.00024.001",site=c("BART","OSBS","SRER","KONZ")) 
neon_store(product="DP1.00024.001", table="PARPAR_30min-expanded")
par_data<-neon_table("PARPAR_30min-expanded")

table(par_data$verticalPosition, par_data$horizontalPosition)
table(par_data$siteID, par_data$verticalPosition)
names(par_data)

table(par_data$horizontalPosition)
## data processing steps
par_data$time<-as_datetime(par$startDateTime)


par<-par_data[,c("time","siteID","PARFinalQF", "PARMean", "verticalPosition")]
head(par)
#3 ok, but the towers have different heights so if we only want the top height, keep em separated.
s40<-par[par$siteID==c("KONZ","SRER"),]
s40<-s40[s40$verticalPosition=="040",]
s60<-par[par$siteID==c("BART","OSBS"),]
s60<-s60[s60$verticalPosition=="060",]
par<-rbind(s40, s60)
table(par$verticalPosition, par$PARFinalQF)

table(par$time, par$siteID)
ggplot(par, aes(x=time, y=PARMean,col=as.factor(PARFinalQF)))+geom_point(shape=".")+facet_wrap(~siteID)
#3 why are there so many lines?
par<-aggregate(list(par=par$PARMean), by=list(time=par$time,siteID=par$siteID), FUN="mean")
ggplot(par, aes(x=time, y=par))+geom_point(shape=".")+facet_wrap(~siteID)
#################

# This downloads the respiration
neon_download(product = "DP1.00095.001",site=c("BART","OSBS","SRER","KONZ")) 
neon_store(product="DP1.00095.001", table="SCO2C_30_minute-expanded")
resp_data<-neon_table("SCO2C_30_minute-expanded")
resp_data$time<-as_datetime(resp_data$startDateTime)
resp<-resp_data[,c("time","siteID","finalQF","verticalPosition","horizontalPosition","soilCO2concentrationMean")]

resp<-aggregate(list(resp=resp$soilCO2concentrationMean), by=list(time=resp$time, siteID=resp$siteID), FUN="mean", na.rm=T)
ggplot(resp, aes(x=time, y=resp))+geom_point(shape=".")+facet_wrap(~siteID)

names(resp)
table(resp$verticalPosition, resp$siteID)
table(resp$horizontalPosition, resp$siteID)


#33#333333
# This downloads the precipitiation
neon_download(product = "DP1.00006.001",site=c("BART","OSBS","SRER","KONZ")) 
table(as.data.frame(neon_index())$table)
neon_store(product="DP1.00006.001", table="THRPRE_30min-expanded")
preci_data<-neon_table("THRPRE_30min-expanded")
preci_data$time<-as_datetime(preci_data$startDateTime)
names(preci_data)
table(preci_data$siteID)

preci<-aggregate(list(preci=preci_data$TFPrecipBulk), by=list(time=preci_data$time, siteID=preci_data$siteID), FUN="mean", na.rm=T)
ggplot(preci, aes(x=time, y=preci))+geom_point(shape=".")+facet_wrap(~siteID)


#33###
# this downloads air temp
neon_download(product = "DP1.00003.001",site=c("BART","OSBS","SRER","KONZ")) 
table(as.data.frame(neon_index())$table)
neon_store(product="DP1.00003.001", table="TAAT_30min-expanded")
temp_data<-neon_table("TAAT_30min-expanded")
temp_data$time<-as_datetime(temp_data$startDateTime)

temp<-aggregate(list(temp=temp_data$tempTripleMean), by=list(time=temp_data$time, siteID=temp_data$siteID), FUN="mean", na.rm=T)
ggplot(temp, aes(x=time, y=temp))+geom_point(shape=".")+facet_wrap(~siteID)

#3333
head(fd)
fd$group<-paste(fd$time, fd$siteID)
temp$group<-paste(temp$time, temp$siteID)
preci$group<-paste(preci$time, preci$siteID)
par$group<-paste(par$time, par$siteID)
resp$group<-paste(resp$time, resp$siteID)


fd$temp<-temp$temp[match(fd$group, temp$group)]
fd$preci<-preci$preci[match(fd$group, temp$group)]
fd$par<-par$par[match(fd$group, temp$group)]
fd$resp<-resp$resp[match(fd$group, temp$group)]

ggplot(fd, aes(x=temp, y=nee))+geom_point(shape=".")+facet_wrap(~siteID)  


write.csv(fd, file="inputs.csv")
