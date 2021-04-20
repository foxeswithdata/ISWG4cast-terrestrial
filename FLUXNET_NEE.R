# script for examining FLUXNET data for use in the EFI challenge, team ISWG
setwd("./FLUXNET data")
library(tidyverse)
library(scales)

# load site data ####
sites = list.files("./", pattern = "_HH_", recursive = TRUE, full.names = TRUE)
load = function(x){
  read.csv(sites[x]) %>% 
    dplyr::select(TIMESTAMP_START, TIMESTAMP_END, NIGHT, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC) %>% 
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
all$TIMESTAMP_START = as.POSIXct(as.character(all$TIMESTAMP_START), format = "%Y%m%d%H%M")
all$TIMESTAMP_END = as.POSIXct(as.character(all$TIMESTAMP_END), format = "%Y%m%d%H%M")
all$day = as.Date(substr(all$TIMESTAMP_START, 1, 10))
all$week = strftime(all$TIMESTAMP_START, format = "%V")
all$NIGHT = as.factor(all$NIGHT)
head(all)

# calculate GPP for day of the year
all_agg = aggregate(data = all, NEE_VUT_REF ~ NIGHT + day + week + site + neon, FUN = mean)
                      # function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
all_agg$week = as.numeric(all_agg$week)
head(all_agg)

# convert NIGHT column and values to different variables
all_night = all_agg %>% 
  filter(NIGHT == 1)
all_night$nee_night = all_night$NEE_VUT_REF
all_night = all_night %>% 
  dplyr::select(day,
                week,
                site,
                neon,
                nee_night)
all_day = all_agg %>% 
  filter(NIGHT == 0)
all_day$nee_day = all_day$NEE_VUT_REF
all_day = all_day %>% 
  dplyr::select(day,
                week,
                site,
                neon,
                nee_day)
all = full_join(all_night, all_day)
all$nee_diff = all$nee_night - all$nee_day

# calculate weekly stats, with combined variation over days of the week and years
night_agg = aggregate(data = all, nee_night ~ week + site + neon, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
day_agg = aggregate(data = all, nee_day ~ week + site + neon, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))
diff_agg = aggregate(data = all, nee_diff ~ week + site + neon, FUN = function(x) c(avg = mean(x), upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))))

# plots
plt = function(df, var, label) {
  ggplot(data = df, aes(x = week, y = var[,1], ymin = var[,3], ymax = var[,2], fill = site)) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    xlab("Week in the Year") +
    ylab(paste0("Mean ", label, " using Variable Ustar Threshold (VUT)"))
}
# nighttime
p1 = plt(df = night_agg, var = night_agg$nee_night, label = "nighttime NEE")
p4 =
  p1 + facet_wrap( ~ neon, nrow = 1)
p4
# daytime
p1 = plt(df = day_agg, var = day_agg$nee_day, label = "daytime NEE")
p4 =
  p1 + facet_wrap( ~ neon, nrow = 1)
p4
# GPP analogue
p1 = plt(df = diff_agg, var = diff_agg$nee_diff, label = "difference in NEE")
p4 =
  p1 + facet_wrap( ~ neon, nrow = 1)
p4

# write data to disk
write.csv(night_agg, "nighttime_weekly_FLUXNET_NEE.csv", row.names = FALSE)
write.csv(day_agg, "daytime_weekly_FLUXNET_NEE.csv", row.names = FALSE)
write.csv(diff_agg, "night-day_weekly_FLUXNET_NEE.csv", row.names = FALSE)
