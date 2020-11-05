


## NEON rooting depth

############ Below uses Neon_store
#remotes::install_github("cboettig/neonstore")
library(neonstore)



# download root biomass 10066.001
neonstore::neon_download(product = "DP1.10066.001", site = c("OSBS","KONZ","BART","SRER"), type = "basic")

## Soil moisture
ind<-as.data.frame(neon_index())
table(ind$product, ind$site)
table(ind$table)

#Load data
neon_store(table = "bbc_rootmass-basic", n = 500) 

d2 <-as.data.frame( neon_read(table = "bbc_rootmass-basic") )

head(d2)
sensor_positions <- neon_table(table = "sensor_positions")




