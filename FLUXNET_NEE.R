# script for examining FLUXNET data for use in the EFI challenge, team ISWG
setwd("./FLUXNET data")
library(tidyverse)
library(scales)

# Harvard Forest- closest to NEON site BART ####
Ha_weekly = read.csv("FLX_US-Ha1_FLUXNET2015_SUBSET_1991-2012_1-4/FLX_US-Ha1_FLUXNET2015_SUBSET_WW_1991-2012_1-4.csv") %>% 
  dplyr::select(TIMESTAMP_START, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
  filter(NEE_VUT_REF_QC != 0)
Ha_weekly$site = "Harvard"
Ks_weekly = read.csv("FLX_US-KS1_FLUXNET2015_SUBSET_2002-2002_1-4/FLX_US-KS1_FLUXNET2015_SUBSET_WW_2002-2002_1-4.csv") %>% 
  dplyr::select(TIMESTAMP_START, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
  filter(NEE_VUT_REF_QC != 0)
Ks_weekly$site = "Kennedy"
Ne_weekly = read.csv("FLX_US-Ne3_FLUXNET2015_SUBSET_2001-2013_1-4/FLX_US-Ne3_FLUXNET2015_SUBSET_WW_2001-2013_1-4.csv") %>% 
  dplyr::select(TIMESTAMP_START, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
  filter(NEE_VUT_REF_QC != 0)
Ne_weekly$site = "Mead"
SR_weekly = read.csv("FLX_US-SRC_FLUXNET2015_SUBSET_2008-2014_1-4/FLX_US-SRC_FLUXNET2015_SUBSET_WW_2008-2014_1-4.csv") %>% 
  dplyr::select(TIMESTAMP_START, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
  filter(NEE_VUT_REF_QC != 0)
SR_weekly$site = "SantaRita"
all_weekly = rbind(Ha_weekly, Ks_weekly, Ne_weekly, SR_weekly)
head(all_weekly)
all_weekly$TIMESTAMP_START = as.Date(as.character(all_weekly$TIMESTAMP_START), format = "%Y%m%d")
all_weekly$week = strftime(all_weekly$TIMESTAMP_START, format = "%V")
head(all_weekly)

# calculate GPP for day of the year
all_agg = aggregate(data = all_weekly, NEE_VUT_REF ~ week + site, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
all_agg$week = as.numeric(all_agg$week)

# plots
p1 = ggplot(data = all_agg, aes(x = week, y = NEE_VUT_REF[,1], ymin = NEE_VUT_REF[,3], ymax = NEE_VUT_REF[,2], fill = site)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  xlab("Week in the Year") +
  ylab("Mean NEE using Variable Ustar Threshold (VUT)")
p4 =
  p1 + facet_wrap( ~ site, nrow = 1)
p4
