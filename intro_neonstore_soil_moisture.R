
library(neonstore)

# trying to access soil moisture sensor locations

library(neonUtilities)
zipsByProduct(dpID="DP1.00002.001", site="BART", 
              startdate="2019-04", enddate="2019-05",
              package="basic", check.size=T)


neon_download(product = "DP1.00094.001",site=c("BART","OSBS","SRER","KONZ")) 
table(as.data.frame(neon_index())$table)
neon_store(product="DP1.00006.001", table="THRPRE_30min-expanded")
preci_data<-neon_table("THRPRE_30min-expanded")
preci_data$time<-as_datetime(preci_data$startDateTime)
names(preci_data)
table(preci_data$siteID)
