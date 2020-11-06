


## NEON rooting depth

############ Below uses Neon_store
#remotes::install_github("cboettig/neonstore")
library(neonstore)





# download root biomass 10066.001
neonstore::neon_download(product = "DP1.10066.001", site = c("OSBS","KONZ","BART","SRER"), type = "basic")

## Soil moisture
ind<-as.data.frame(neon_index())

table(ind$table)
table(ind$product, ind$site)


#Load data
neon_store(table = "bbc_rootmass-basic") 


rbio <-as.data.frame( neon_read(table = "bbc_rootmass-basic") )
rbio<-rbio[rbio$siteID==c("BART","KONZ","SRER","OSBS"),]


roots <-loadByProduct(
  site = c("BART","KONZ","OSBS","SRER"),
  dpID = "DP1.10066.001",
  package = "basic",
  check.size = F )
names(roots)
roots$variables_10066

head(roots$mpr_perrootsample)

#####################################################

##Vegetation biomass

tree <-
  loadByProduct(
    site = c("BART","OSBS","SRER","KONZ"),
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE  )

chx<-tree$vst_apparentindividual
chx
location<-tree$vst_mappingandtagging


chx$species<-location$taxonID[match(chx$individualID, location$individualID)]


plots<-aggregate(chx$stemDiameter, by=list(siteID=chx$siteID, plotID=chx$plotID), FUN="sum", na.rm=T)

ggplot(plots, aes(x=siteID, y=x))+geom_boxplot()+scale_y_log10()








# you can get locations of veg plots conveniently using getLocTOS
m <- getLocTOS(data = tree$vst_mappingandtagging,
               dataProd = "vst_mappingandtagging")
m

table(chx$eventID)
table(m$namedLocation)
# calculate the mean easting and northing for the trees. this gets plot centers (to use for lidar)
locMean <-
  aggregate(
    list(
      adjNorthing = m$adjNorthing ,
      adjEasting = m$adjEasting
    ),
    by = list(plotID = m$plotID),
    FUN = mean,
    na.rm = T
  )


head(locMean,120)

##
byTileAOP("DP3.30010.001", site=c("SRER","KONZ","SRER",'BART'), year="2019", 
          check.size = F,buffer = 200,
          easting=locMean$adjEasting, northing=locMean$adjNorthing , savepath="neon_downloads")












