# script for examining FLUXNET data for use in the EFI challenge, team ISWG
setwd("./FLUXNET data")
library(tidyverse)
library(scales)

# load site data ####
sites = list.files("./", pattern = "_WW_", recursive = TRUE, full.names = TRUE)
load = function(x){
  read.csv(sites[x]) %>% 
    dplyr::select(TIMESTAMP_START, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
    filter(NEE_VUT_REF_QC != 0)
}
arc = load(1)
arc$site = "ARM"
arc$neon = "KONZ"
ha = load(2)
ha$site = "Harvard"
ha$neon = "BART"
ks = load(3)
ks$site = "Kennedy"
ks$neon = "OSBS"
lww = load(4)
lww$site = "Washita"
lww$neon = "KONZ"
src = load(5)
src$site = "SRC"
src$neon = "SRER"
srg = load(6)
srg$site = "SRG"
srg$neon = "SRER"
srm = load(7)
srm$site = "SRM"
srm$neon = "SRER"

all = rbind(arc, ha, ks, lww, src, srg, srm)
head(all)
all$TIMESTAMP_START = as.Date(as.character(all$TIMESTAMP_START), format = "%Y%m%d")
all$week = strftime(all$TIMESTAMP_START, format = "%V")
head(all)

# calculate GPP for day of the year
all_agg = aggregate(data = all, NEE_VUT_REF ~ week + site + neon, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
all_agg$week = as.numeric(all_agg$week)
head(all_agg)

# plots
p1 = ggplot(data = all_agg, aes(x = week, y = NEE_VUT_REF[,1], ymin = NEE_VUT_REF[,3], ymax = NEE_VUT_REF[,2], fill = site)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  xlab("Week in the Year") +
  ylab("Mean NEE using Variable Ustar Threshold (VUT)")
p4 =
  p1 + facet_wrap( ~ neon, nrow = 1)
p4

write.csv(all_agg, "weekly_FLUXNET_NEE.csv", row.names = FALSE)
