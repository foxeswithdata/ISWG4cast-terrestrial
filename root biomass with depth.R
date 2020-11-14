



library(neonUtilities)

### this worked well.  
roots <-loadByProduct(site = c("BART","KONZ","OSBS","SRER"),
  dpID = "DP1.10066.001",  check.size = F )


names(roots)

names(roots$bbc_rootmass)

names(roots$categoricalCodes_10066)
names(roots$mpr_carbonNitrogen)
names(roots$mpr_perdepthincrement )
names(roots$mpr_perpitprofile)
names(roots$mpr_perrootsample)

head(ro)
ro<-roots$mpr_perrootsample
ro$siteID<-substr(ro$sampleID, 1, 4)

# I like the one below more.
ggplot(ro, aes(x=sizeCategory,y=rootDryMass, col=rootStatus))+geom_point()+
  facet_wrap(~siteID,scales="free_x")+ggtitle("Root Biomass")+  theme(axis.text.x = element_text(angle = 90))



rbio<-roots$mpr_perpitprofile
head(rbio)

library(ggplot2)

g.2<-ggplot(rbio, aes(x=totalRootBiomass, y=-maxProfileDepth, shape=sizeCategory ,col=rootStatus))+geom_point()+
  facet_wrap(~siteID, nrow=1)+ggtitle("Root Biomass")



library(ggpubr)
ggarrange(g.1, g.2, g.3, g.4)


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
head(chx)

plots<-aggregate(chx$stemDiameter, by=list(siteID=chx$siteID, plotID=chx$plotID), FUN="sum", na.rm=T)

g.1<-ggplot(chx, aes(x=plotID, y=stemDiameter, col=siteID))+geom_boxplot()+scale_y_log10()+facet_wrap(~siteID, scales="free_x", nrow=1)+
  theme(axis.text.x = element_text(angle = 90))+ggtitle("Tree diameters")
g.1




########### don't go below yet.

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












