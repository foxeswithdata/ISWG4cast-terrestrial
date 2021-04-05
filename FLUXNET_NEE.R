# script for examining FLUXNET data for use in the EFI challenge, team ISWG
setwd("D:/ISWG")
library(tidyverse)
library(scales)

# Harvard Forest- closest to NEON site BART ####
Ha_daily = read.csv("FLX_US-Ha1_FLUXNET2015_SUBSET_1991-2012_1-4/FLX_US-Ha1_FLUXNET2015_SUBSET_WW_1991-2012_1-4.csv") %>% 
  dplyr::select(TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF)
Ha_daily$site = "Harvard"
Ks_daily = read.csv("FLX_US-KS1_FLUXNET2015_SUBSET_2002-2002_1-4/FLX_US-KS1_FLUXNET2015_SUBSET_DD_2002-2002_1-4.csv") %>% 
  dplyr::select(TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF)
Ks_daily$site = "Kennedy"
Ne_daily = read.csv("FLX_US-Ne3_FLUXNET2015_SUBSET_2001-2013_1-4/FLX_US-Ne3_FLUXNET2015_SUBSET_DD_2001-2013_1-4.csv") %>% 
  dplyr::select(TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF)
Ne_daily$site = "Mead"
SR_daily = read.csv("FLX_US-SRC_FLUXNET2015_SUBSET_2008-2014_1-4/FLX_US-SRC_FLUXNET2015_SUBSET_DD_2008-2014_1-4.csv") %>% 
  dplyr::select(TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF)
SR_daily$site = "SantaRita"
all_daily = rbind(Ha_daily, Ks_daily, Ne_daily, SR_daily)
head(all_daily)
all_daily$TIMESTAMP = as.Date(as.character(all_daily$TIMESTAMP), format = "%Y%m%d")
all_daily$dayofyear = paste0(substring(all_daily$TIMESTAMP, 6, 7), substring(all_daily$TIMESTAMP, 9, 10))

# calculate GPP for day of the year
all_agg = aggregate(data = all_daily, GPP_NT_VUT_REF ~ dayofyear + site, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
all_agg2 = aggregate(data = all_daily, GPP_DT_VUT_REF ~ dayofyear + site, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
all_agg = merge(all_agg, all_agg2)
all_agg = pivot_longer(data = all_agg, cols = c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF"), names_to = "GPP")
all_agg$dayofyear = as.POSIXct(as.character(all_agg$dayofyear), format = "%m%d")

# plots
p1 = ggplot(data = all_agg, aes(x = dayofyear, y = value[,1], ymin = value[,3], ymax = value[,2], fill = GPP)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  xlab("Day of Year") +
  ylab("GPP by partitioning method") +
  scale_x_datetime(labels = date_format("%b"))
p4 =
  p1 + facet_wrap( ~ site, nrow = 1)
p4
