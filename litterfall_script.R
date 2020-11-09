




##Vegetation biomass

litterfall <-
  loadByProduct(
    site = c("BART","OSBS","SRER","KONZ"),
    dpID = "DP1.10033.001",
    package = "basic",
    check.size = FALSE  )

zipsByProduct(dpID="DP1.10033.001", site=c("BART","OSBS","SRER","KONZ"), 
              startdate="2016-01", enddate="2019-06",
              package="basic", check.size=T,filepath="neon_downloads")

ltr<-stackByTable(filepath="C:/Users/Dropcopter2/Documents/GitHub/ISWG4cast-terrestrial/filesToStack10033")




###############################################################

# download root biomass 10066.001
# litterfall and woody debris DP1.10033.001
neonstore::neon_download(product = "DP1.10033.001", site = c("OSBS","KONZ","BART","SRER"), type = "basic")

ind<-as.data.frame(neon_index())
table(ind$table)


#Load data
ltr <-as.data.frame( neon_read(table = "ltr_massdata-basic") )

head(ltr)
table(ltr$siteID)

library(ggplot2)
dev.off()
ggplot(ltr, aes(x=functionalGroup, y=drymass))+geom_jitter(width=.2)+facet_wrap(~siteID)


###########################################

names(litterfall)

chx<-tree$vst_apparentindividual
chx
location<-tree$vst_mappingandtagging


chx$species<-location$taxonID[match(chx$individualID, location$individualID)]


plots<-aggregate(chx$stemDiameter, by=list(siteID=chx$siteID, plotID=chx$plotID), FUN="sum", na.rm=T)

ggplot(plots, aes(x=siteID, y=x))+geom_boxplot()+scale_y_log10()
