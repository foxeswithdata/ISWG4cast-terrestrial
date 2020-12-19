library(broom)
library(ggplot2)
library(neonUtilities)
library(devtools)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
library(lidR)
library(gstat)
library(rgeos)
library(data.table)
library(sp)
library(rgdal)
library(raster)

# soil characteristics
phys <-
  loadByProduct(
    site = c("BART","SRER","OSBS","KONZ"),
    dpID = "DP1.10047.001",
    package = "basic",
    check.size = FALSE
  )
names(phys)
# this is the data page of the spc object
data <- phys$spc_biogeochem

# make separate objects for each df.
bd <- phys$spc_bulkdensity
ph <- phys$spc_perhorizon
ph$horizonHeight <- ph$horizonBottomDepth - ph$horizonTopDepth

# choose the data columns you want to use
use <- data[, c(
  "domainID",
  "horizonName",
  "siteID",
  "plotID",
  "horizonID",
  "carbonTot",
  "nitrogenTot",
  "ctonRatio",
  "estimatedOC"
)]

#bring in data from other objects
use$bulkDensFieldMoist <-
  bd$bulkDensFieldMoist[match(use$horizonID, bd$horizonID)]
use$bulkDensThirdBar <-
  bd$bulkDensThirdBar[match(use$horizonID, bd$horizonID)]
use$horizonThickness <-
  ph$horizonHeight[match(use$horizonID, ph$horizonID)]


# start by creating the column you want to add data to, then go by row and calculate the values. Here, I've created a function to test if the default calculation leads to Na's
# dummy column
use$carbonTot_stock = NA
use$nitrogenTot_stock = NA
# options for calculations
option1 = function(row, column){
  use[row,column] * use$bulkDensFieldMoist[row] * use$horizonThickness[row] / 1000
}
option2 = function(row, column){
  use[row,column] * use$bulkDensThirdBar[row] * use$horizonThickness[row] / 1000
}

# for every row in "use", check the following conditions
for(i in c(1:nrow(use))){
# for both rows carbonTot and NitrogenTot, check the following conditions
  for(a in c(6, 7)){
    if(is.na(option1(i, a)) == FALSE){
      # if the horizon is Oa, do this calculation
      use$carbonTot_stock[i] = option1(i, a)
      use$nitrogenTot_stock[i] = option1(i, a) #AY added this
    } else {
      # otherwise, do this calculation
      use$carbonTot_stock[i] = option2(i, a)
      use$nitrogenTot_stock[i] = option2(i, a) #AY added this
    }
  }
}


# 'stock' is a df of the C stock as calculated above
stock <-
  aggregate(
    list(
      Nstock = use$nitrogenTot_stock,
      Cstock = use$carbonTot_stock
    ),
    by = list(plotID = use$plotID, siteID=use$siteID),
    FUN = "sum",
    na.rm = T
  )

ggplot(use, aes(x=siteID, y=nitrogenTot_stock, col=siteID))+geom_point()

use[use$siteID=="SRER",]
# good ol' C:N ratio
ggplot(stock, aes(x = Cstock, y = Nstock, col=siteID)) + geom_point()

## now access plotID locations-
# begin by loading veg plot
## You could use soil sensor location or soil plot location... I just used trees for convenience
tree <-
  loadByProduct(
    site = "BART",
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE
  )

# you can get locations of veg plots conveniently using getLocTOS
m <- getLocTOS(data = tree$vst_mappingandtagging,
               dataProd = "vst_mappingandtagging")

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


head(locMean)

# now bring it into the soil stock dataframe by matching plotID
stock$Northing <-
  locMean$adjNorthing[match(stock$plotID, locMean$plotID)]
stock$Easting <-
  locMean$adjEasting[match(stock$plotID, locMean$plotID)]



head(stock, 50)
stock <- na.omit(stock) # if there are pits with NA for stock, we don't want them.
easting <- stock$Easting
northing <- stock$Northing

###! Lidar!!
# You can download the .laz files using byTile AOP for the specified easting and northing
## It'll ask you if its ok to download the file size (press y into console)
 byTileAOP("DP1.30003.001", site="BART", year="2017", check.size = T,buffer = 200, easting=easting, northing=northing, savepath="C:\\Users\\Dropcopter2\\Documents\\R\\EFI\\Downloads")

# this is a list of .laz files that are available from the 'Airborne Observation Platform' data product.
file_list <-list.files(path = "C:\\Users\\Dropcopter2\\Documents\\R\\EFI\\Downloads\\DP1.30003.001\\2017\\FullSite\\D01\\2017_BART_3\\L1\\DiscreteLidar\\ClassifiedPointCloud")
file_list


#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()


setwd("C:\\Users\\Dropcopter2\\Documents\\R\\EFI\\Downloads\\DP1.30003.001\\2017\\FullSite\\D01\\2017_BART_3\\L1\\DiscreteLidar\\ClassifiedPointCloud")
laz <- readLAS(file_list)


############## The loop to calculate complexity metrics for area surrounding each soil pit #################################################
head(stock)
dim(stock)
centroids <- stock[, c("Northing", "Easting")]

plot.metrics <- list()
for (i in c(1:length(centroids))) {
  center <- centroids[i, ]
  use.laz <- la # this is where I set it to the 'C1laz' file.
  
  # You shouldn't have to change anything below.
  # select the xy location of the plot we are calculating canopy metrics for
  x <- as.numeric(center[1])
  y <- as.numeric(center[2])
  
  #Cut out a 200 x 200 m buffer by adding 100 m to easting and northing coordinates (x,y).
  data.cut <-
    lasclipRectangle(
      use.laz ,
      xleft = (as.numeric(x - 100)),
      ybottom = (as.numeric(y - 100)),
      xright = (as.numeric(x + 100)),
      ytop = (as.numeric(y + 100))
    )
  #Correct for ground height using a kriging function to interpolate elevation from ground points in the .laz file.
  #If the function will not run, then you may need to checkfor outliers by adjusting the 'drop_z_' arguments when reading in the .laz files.
  dtm <- grid_terrain(data.cut, 1, kriging(k = 10L))
  data.200m <- lasnormalize(data.cut, dtm)
  plot(data.200m)

  ## Alex chose a 40 x 40 m area for the trees surrounding a soil plot
  data.30m <-
    lasclipRectangle(
      data.200m,
      xleft = (x - 20),
      ybottom = (y - 20),
      xright = (x + 20),
      ytop = (y + 20)
    )
  data.30m@data$Z[data.30m@data$Z <= .5] <- NA
  laz_data <- data.30m
  plot(laz_data)
  structural_diversity_metrics <- function(laz_data) {
    chm <- grid_canopy(laz_data, res = 1, dsmtin())
    mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE)
    max.canopy.ht <- max(chm@data@values, na.rm = TRUE)
    rumple <- rumple_index(chm)
    top.rugosity <- sd(chm@data@values, na.rm = TRUE)
    cells <- length(chm@data@values)
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0
    zeros <- which(chm.0@data@values == 0)
    deepgaps <- length(zeros)
    deepgap.fraction <- deepgaps / cells
    cover.fraction <- 1 - deepgap.fraction
    vert.sd <- cloud_metrics(laz_data, sd(Z, na.rm = TRUE))
    sd.1m2 <- grid_metrics(laz_data, sd(Z), 1)
    sd.sd <- sd(sd.1m2[, 3], na.rm = TRUE)
    Zs <- laz_data@data$Z
    Zs <- Zs[!is.na(Zs)]
    entro <- entropy(Zs, by = 1)
    gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3)
    GFP.AOP <- mean(gap_frac$gf)
    LADen <- LAD(Zs, dz = 1, k = 0.5, z0 = 3)
    VAI.AOP <- sum(LADen$lad, na.rm = TRUE)
    VCI.AOP <- VCI(Zs, by = 1, zmax = 100)
    out.plot <- data.frame(matrix(
      c(x,
        y,
        mean.max.canopy.ht,
        max.canopy.ht,
        rumple,
        deepgaps,
        deepgap.fraction,
        cover.fraction,
        top.rugosity,
        vert.sd,
        sd.sd,
        entro,
        GFP.AOP,
        VAI.AOP,
        VCI.AOP
      ),
      ncol = 15
    ))
    colnames(out.plot) <-
      c(
        "easting",
        "northing",
        "mean.max.canopy.ht.aop",
        "max.canopy.ht.aop",
        "rumple.aop",
        "deepgaps.aop",
        "deepgap.fraction.aop",
        "cover.fraction.aop",
        "top.rugosity.aop",
        "vert.sd.aop",
        "sd.sd.aop",
        "entropy.aop",
        "GFP.AOP.aop",
        "VAI.AOP.aop",
        "VCI.AOP.aop"
      )
    print(out.plot)
  }
  plot.metrics[[i]] <- structural_diversity_metrics(laz_data)
}  # END LOOP


## From here Alex thinks he has more code to merge the plot-level soil data to the plot-level forest complexity metrics.
#### Should be possible to merge using the plotIDs!