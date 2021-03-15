

#### get daily met data, read in flux data


library(daymetr)



bart<-download_daymet(
  lat = 44.063889, 
  lon = -71.287375,
  start = 2000,
  end = 2020,
  internal = TRUE)
BART<-bart$data
BART$siteID<-"BART"

konz<-download_daymet(
  lat = 39.100774,  
  lon = -96.563075,
  start = 2000,
  end = 2020,
  internal = TRUE)
KONZ<-konz$data
KONZ$siteID<-"KONZ"

osbs<-download_daymet(
  lat = 29.689282,   
  lon = -81.993431,
  start = 2000,
  end = 2020,
  internal = TRUE)
OSBS<-osbs$data
OSBS$siteID<-"OSBS"


srer<-download_daymet(
  lat = 31.91068,    
  lon = -110.83549,
  start = 2000,
  end = 2020,
  internal = TRUE)
SRER<-srer$data

SRER$siteID<-"SRER"
dim(SRER)
dim(BART)
dim(OSBS)
dim(SRER)

daymet<-rbind(BART, OSBS, SRER, KONZ)
write.csv(daymet, file="meriflux/Daymet_2000_2020.csv")

####

bt<-read.csv("AMF_US-SP1_BASE-BADM_4-1/AMF_US-Bar_BASE_HH_5-5.csv")
names(bt)

b<-bt[, c("TIMESTAMP_START","USTAR_1_1_1","LE_1_1_1","CO2_1_1_1","NETRAD_1_1_1")]




kt<-read.csv("meriflux/Konza_tower.csv")
names(kt)
k<-kt[,c("TIMESTAMP_START","USTAR_1_1_1","LE_1_1_1","CO2_1_1_1","NETRAD_1_1_1")]

st<-read.csv("meriflux/Santa_Rita_tower.csv")
names(st)
s<-st[,c("TIMESTAMP_START","USTAR","LE","CO2","NETRAD"  )]


ot<-read.csv("meriflux/Ordway_tower.csv")
names(ot)
o<-ot[,c("TIMESTAMP_START","USTAR","LE","CO2_1","NETRAD"  )]


# time- YYYYMMDDHHMM
head(b)
b[1,1]

cols<-c("TIMESTAMP_START","USTAR","LE","CO2","NETRAD"  )


colnames(b)<-cols
colnames(o)<-cols
colnames(s)<-cols
colnames(k)<-cols

b$siteID<-"BART"
o$siteID<-"OSBS"
k$siteID<-"KONZ"
s$siteID<-"SRER"


meriflux<-rbind(b,o,s,k)

write.csv(meriflux, file="meriflux/combined_towers.csv")
