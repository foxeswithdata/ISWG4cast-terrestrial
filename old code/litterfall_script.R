


library(neonstore)
library(lubridate)

##litterfall biomass

litterfall <-
  loadByProduct(
    site = c("BART","OSBS","SRER","KONZ"),
    dpID = "DP1.10033.001",
    package = "basic",
    check.size = FALSE  )


names(litterfall)
names(litterfall$ltr_pertrap)
names(litterfall$ltr_massdata)
mass<-litterfall$ltr_massdata
mass$Year<-year(mass$collectDate)

g.3<-ggplot(mass, aes(x=plotID, y=dryMass,col=functionalGroup))+geom_jitter(width=.2)+facet_wrap(~siteID, scales="free_x", nrow=1)+
  theme(axis.text.x = element_text(angle = 90))+ggtitle("Litterfall biomass")




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

use[use$carbonTot_stock==0,]

table(use$carbonTot_stock>0, use$siteID)

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


head(use)
g.4<-ggplot(stock, aes(x=plotID, y=Cstock, col=siteID))+geom_point()+facet_wrap(~siteID, scales="free_x", nrow=1)+
  theme(axis.text.x = element_text(angle = 90))+ggtitle("Total stock carbon in soil")
g.4





########## Woody debris

DP1.10010.001
