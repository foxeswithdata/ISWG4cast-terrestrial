
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(lubridate)
library(contentid)
library(data.table)
library(ggplot2)

# Thanks to EFI, the target variables that have been passed through a QA filter are available here. 
#The NEE, LE, and Soil moisture are contained with this dataet. 

half<-fread("march_terrestrial_30min-targets.csv.gz")
daily<-fread("march_terrestrial_daily-targets.csv.gz")


tail(daily) # goes to December 2020

daily$month<-month(daily$time)
daily$day<-day(daily$time)
daily$year<-year(daily$time)

dec<-daily[daily$month=="12",]
jan<-daily[daily$month=="1",]
feb<-daily[daily$month=="2",]
mar<-daily[daily$month=="3",]
apr<-daily[daily$month=="4",]

ggplot(daily, aes(x=month, y=nee ))+geom_jitter()+facet_wrap(~siteID)+ggtitle("NEE")
ggplot(daily, aes(x=month, y=le ))+geom_jitter()+facet_wrap(~siteID)+ggtitle("LE")
ggplot(daily, aes(x=month, y=vswc ))+geom_jitter()+facet_wrap(~siteID)+ggtitle("soil moisture")


wind<-rbind(mar, feb, apr)
ja<-gather(wind,"target","value",3:5)

a<-ggplot(ja, aes(x=day, y=value, shape=as.factor(month), col=as.factor(year)))+geom_point()+
  facet_grid(target~siteID, scales="free_y")+ggtitle("Daily")
a
############

## Now for half hourly data

ha<-gather(half,"target","value",c(3,4,11))
names(ha)
ha<-ha[ ,c(1,2,10,11)]

ha$month<-month(ha$time)
ha$day<-day(ha$time)
ha$year<-year(ha$time)
head(ha,200)

Jan<-ha[ha$month==1,]
Jan<-ha[ha$month==2,]
Mar<-ha[ha$month==3,]



ggplot(ha, aes(x=month, y=value, col=siteID ))+geom_jitter()+facet_grid(target~siteID, scales="free")






b<-ggplot(Mar, aes(x=day, y=value, shape=as.factor(month), col=as.factor(year)))+geom_point()+
  facet_grid(target~siteID, scales="free_y")+ggtitle("Half-hour")
b

## calculate average day from the half hours
da<-aggregate(list(value=Jan$value), by=list(siteID=Jan$siteID, target=Jan$target,day=Jan$day),FUN="mean", na.rm=T)
head(da)

# do standard deviation! for each day
st.err <- function(x) {  sd(x, na.rm=T)  }
SE <- aggregate( list(se=Jan$value), by=list(siteID=Jan$siteID, target=Jan$target,day=Jan$day),FUN=  st.err)
head(SE)
da$se<-SE$se


c<-ggplot(da, aes(x=day, y=value))+  geom_point()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2)+
  facet_grid(target~siteID, scales="free_y")+ggtitle("Avg+SD of half hour historical January data")
c



##### MONTH AVG
month<-aggregate(list(value=da$value), by=list(siteID=da$siteID,target=da$target),FUN="mean", na.rm=T)

## Here is where I might want to do 95% confidence?
mse<-aggregate(list(se=da$se), by=list(siteID=da$siteID, target=da$target),FUN="mean", na.rm=T)
head(mse)
head(month)


month$site.target<-paste(month$siteID, month$target)
mse$site.target<-paste(mse$siteID, mse$target)

month$se<-mse$se[match(month$site.target, mse$site.target)]

ja$site.target<-paste(ja$siteID, ja$target)

head(ja)

# match in the monthly values into ja
ja$m.avg<-month$value[match(ja$site.target, month$site.target)]
ja$se<-month$se[match(ja$site.target, mse$site.target)]

# only 1 year
y21<-ja[ja$year=="2020",]

y21
d<-ggplot(y21, aes(x=day, y=m.avg))+  geom_point()+
  geom_errorbar(aes(ymin=m.avg-se, ymax=m.avg+se), width=.2)+
  facet_grid(target~siteID, scales="free_y")+ggtitle("Monthly average from half hours")
d


# see the 4 options and workflow
library(ggpubr)
ggarrange(a,b,c,d, common.legend = T, nrow=1)


######### Alright, now format the data
names(ja)
us<-y21
use<-us[,c(1,2,5,6,7,10)]
use.se<-us[,c(1,2,5,6,7,11)]



stat.mean<-spread(use[,c(1,2,5,6)], "target","m.avg")
stat.mean$statistic<-"mean"
stat.mean[stat.mean$siteID=="BART","vswc"]<-"0"
head(stat.mean,20)

names(use.se)

stat.se<-spread(use.se[,c(1,2,5,6)], "target","se")
stat.se$statistic<-"sd"
stat.se[stat.se$siteID=="BART","vswc"]<-"0.05"
head(stat.se)

fin<-rbind(stat.mean, stat.se)
fin$forecast<-"1"
fin$data_assimilation<-"0"
names(fin)
head(fin)

## pick which rows you want to keep using
#final<-fin[,c("siteID","statistic","forecast","data_assimilation","time","nee","le","vcwc")]
dim(fin)
names(fin)
final<-fin[,c(2,6,7,8,1,4,3,5)]

# too many rows!
fin<-final[1:144,]
table(fin$time)


final[final$statistic=="sd",]

fins<-final[241:384,]
table(fins$time)

fi<-rbind(fin, fins)
head(fi)

fi$time<-ymd(fi$time)
fi$year<-2021
fi$month<-month(fi$time)
fi$day<-day(fi$time)

fi$date<-paste(fi$year, fi$month, fi$day, sep="-")
head(fi)

fi$time<-ymd(fi$date)

names(fi)
fi<-fi[,1:8]

head(fi)
table(fi$time)
tail(fi)
table(fi$statistic)

write.csv(fi, file="terrestrial-2021-02-01-ISWG.csv")


git config --global user.email "bearsofthemoss@gmail.com "
git config --global user.name "Alex Y"
