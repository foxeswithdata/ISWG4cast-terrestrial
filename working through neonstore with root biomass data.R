
## Intro to using neonstore by Carl Boetigger: https://github.com/cboettig/neonstore

remotes::install_github("cboettig/neonstore")
library(neonstore)
###  This is a way to access all the products.
products <- neon_products()

i <- grepl("Populations", products$themes)
prod<-as.data.frame(products[i, c("productCode", "productName")])

print(prod)  # I don't see a soil physical properties - from neonUtilities it is dpID: "DP1.10047.001"

# this is to download the root biomass, chemistry, periodic
neon_download("DP1.10067.001")

# look at the files you've downloaded
ind<-as.data.frame(neon_index())
table(ind$product, ind$site)

# view what table names you have?
table(ind$table)

# specify a table name to read
rb<-neon_read(table="bbc_rootmass-basic")
table(rb$siteID, rb$collectDate)


library(neonstore)
library(dplyr)
neon_products() %>% filter(grepl("[Ss]oil", productName))


neon_download("DP1.10047.001")
neon_store("DP1.10047.001") # import into a local database



#############
neon_download("DP1.10047.001")
# See what you have downloaded
ind<-as.data.frame(neon_index())
head(ind)
# view what table names you have?
table(ind$table)

# read in data?
bgc<-neon_read("spc_biogeochem-basic") # import into a local database

#####################################
## Soil moisture

#Load data
neon_store(table = "SWS_30_minute", n = 500) 
d2 <- neon_read(table = "sensor_positions") 
sm30 <- neon_table(table = "SWS_30_minute")
sensor_positions <- neon_table(table = "sensor_positions")



