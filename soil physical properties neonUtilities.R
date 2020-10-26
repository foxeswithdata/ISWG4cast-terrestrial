library(plotrix)
library(neonUtilities)



## Below, use data stored in the soils library to make a soil texture plot.
###   information on the plotrix soil texture triangles: https://rdrr.io/cran/plotrix/man/soil.texture.html

### soil physical chx
phys <-
  loadByProduct(
    site = c("BART","KONZ","OSBS","SRER"),
    dpID = "DP1.10047.001",
    package = "basic",
    check.size = FALSE
  )

names(phys)  # here you see the various 'slots' of the object for dp 10047.

# make a soil texture object 'txtr'
pre.txr<-phys$spc_particlesize
#  choose columns you want
txr<-pre.txr[,c(4,5,8,9,20,21,22)]
txr<-na.omit(txr)
head(txr)

## make a two by two panel graph!
par(mfrow=c(2,2))
soil.texture(txr[txr$siteID=="BART",5:7], main="BART", show.grid=FALSE, show.lines=T, show.names = F)
soil.texture(txr[txr$siteID=="OSBS",5:7], main="OSBS", show.grid=FALSE, show.lines=T, show.names = F)
soil.texture(txr[txr$siteID=="KONZ",5:7], main="KONZ", show.grid=FALSE, show.lines=T, show.names = F)
soil.texture(txr[txr$siteID=="OSBS",5:7], main="OSBS", show.grid=FALSE, show.lines=T, show.names = F)


######## 'dig into the phys object

names(phys)

names(phys$spc_perplot)
names(phys$spc_perhorizon)
phys$readme_10047


## this object is the new 10,086 object that has mulitple soil data products in it.  I find it more confusing.
new <-
  loadByProduct(
    site = c("BART","KONZ","OSBS","SRER"),
    dpID = "DP1.10086.001",
    package = "basic",
    check.size = FALSE
  )


names(new)  # see how the slots differ?
names(phys)
new$readme_10086

new$sls_bgcSubsampling
new$sls_metagenomicsPooling
new$sls_soilCoreCollection
new$sls_soilMoisture

names(new$variables_10086)
names(new$spc_bulkdensity)
names(new$spc_particlesize)

