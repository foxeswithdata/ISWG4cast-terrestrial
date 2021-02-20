


l<-read.csv("C:\\Users\\Dropcopter2\\Downloads\\16S_QIIME2_draft.csv")

table(l$)
head(l)
div_1m2Data



l$SiteID<-str_sub(l$Plot,1,4)
table(l$Site)

sites<-unique(l$Site)

sites

library(neonstore)



library(neonUtilities)
library(geoNEON)
library(stringr)


# load the percent cover data
trees <-loadByProduct(site = sites,dpID = "DP1.10098.001",package = "basic",check.size = FALSE)

names(percent)


# you can get locations of veg plots conveniently using getLocTOS
m <- getLocTOS(data = percent$div_1m2Data,
               dataProd = "div_1m2Data")
head(m)
 

write.csv(m, file="percent_cover_plot_info.csv")

write.csv(percent$div_1m2Data, file="percent_cover_data.csv" )

p<-unique(percent$div_1m2Data$plotID)
ploc<-unique(m$plotID)


stat<-as.data.frame(table(m$siteID, m$nlcdClass))
head(stat)

stat<-stat[stat$Freq>0,]
head(stat)
library(ggplot2)
stat$log.Freq<-log(stat$Freq)
a1<-ggplot(stat, aes(Var2, Var1 )) +
  geom_tile(aes(fill = log.Freq)) +theme_classic()
a1
  scale_fill_gradientn(colours = c("white", "light green", "forest green"))+
  stat_bin2d(geom="text", aes(label=Freq), size=54)+coord_fixed()+  
scale_x_discrete(position = "top") 
a1


table(a)

head(l)

phyt_plot<-l$Plot_ID %in% p
table(phyt_plot)
head(percent$div_1m2Data)



