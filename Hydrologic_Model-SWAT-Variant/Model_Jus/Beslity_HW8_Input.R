#Justin Beslity
#Hydrologic Modeling
#HW #8a
#4/24/2019

#Purpose of Script: 

# 1) Calculate Curve Number (CN) for each day using Haith's smoothed curve number approach
# 2) 5-day Andecedent Moisture
#     AM 5 = Sum of R + M (i = t-5 to t-1)
#       R = Rain
#       M = Melt
# 3) Calculate Surface Runoff (SR)
# 4) Snow Pack (SN)
#     If T <= 0 SNt+1 = SN + P, R = 0, M = 0
#     If T >= 0 R = P, M = K*T or max SN, SNt+1 = SN - M
# 5) Unsaturated Storage (UNSAT)
#     UNSATt+1 = UNSAT + I - ET - PERC
#       If UNSATt+1 <= 10, PERC = 0
#       If UNSATt+1 >= 10, PERC = UNSATt+1 - 10, UNSATt+1 = 10
#         PERC = Percolation
# 5) Infiltration (I)
#       I = R + M - SR
# 6) Evapotranspiration (ET)
#     ET = 0.021*N^2*esat/(T+273)
#       N = Number of hours of daylight 
#       esat = Saturated water vapor pressure (mb) (function of (T))
#       T = Avg T
# 7) Actual ET (AET)
#     AET = ET*KU
#       KU = Crop Cover Coeficient
# 8) Subsurface Drainage (SD)
#     SD = (1-Kb)*SAT
# 9) Saturated Storage (SAT)
#     SATt+1 = SAT + PERC - SD
# 10) Estimated Streamdflow (Q)
#      Q = SR + SD
# 11) Atweb ET Model
#       Solar Radiation (Rs) MJ/day
#       Latent Heat of Vaporization (lamda) = 2.45 MJ/kg
#       Dimensionless Coefficient (K) = 0.53
#    k*(Rs/lamda) = PET (mm/day)

#Data:

#Part 1:
#flow - Mean Daily Stream Flow data for Genessee River
#met_data - Filled precipitation, Tmin and Tmax data

rm(list=ls())
graphics.off()

time1 <- proc.time()
#dev.off()
#detach()
library(zoo)
library(lubridate)
#install.packages("lubridate")
#install.packages("data.table")
library(data.table)
#install.packages("dyplr")
library(dplyr)
source("Hydrologic_Model-SWAT-Variant/Model_Jus/Beslity_HW2_Functions-Script.R")

######Part 1########

met_data <- read.csv("Hydrologic_Model-SWAT-Variant/Model_Jus/Met_Data_HW7_Beslity.csv", header = TRUE)
met_data$DATE <-as.Date(met_data$DATE, format = "%m/%d/%Y")

#####Part 2######

#Average Tmin and Tmax for each day
met_data$TAVG <- (met_data$TMAX+met_data$TMIN)/2

#Convvert to Celcius
met_data$TAVG_C <- (met_data$TAVG-32)*(5/9)


#Add Saturated Vapor Pressure for each day
met_data$esat <- 6.108*exp((17.27*met_data$TAVG_C)/(237.3+met_data$TAVG_C))

#Create mothly daylight hour dataframe to query in for loop
daylight <- data.frame(c(1:12), 
                       c(9.3, 10.4, 11.7, 13.1, 14.3, 15.0, 14.6, 13.6, 12.3, 10.9, 9.7, 9.0))
colnames(daylight) <- c("mon", "day_hours")

#Merge daylight hours and weather data
met_data <- merge.data.frame(x = met_data, y = daylight, by.x = "mon", all.y = T)
met_data<-met_data[order(met_data$DATE),]
met_data <- arrange(met_data, .by_group = met_data$DATE)


#Calculate Potential ET using the Hamon method
met_data$ET <- NA
for(i in seq(1:nrow(met_data))){
  if (met_data[i,9] <= 0){
    met_data[i,12] = 0
  }
  else {
    met_data[i,12] = (0.021*((met_data[i,11])^2)*met_data[i,10])/(met_data[i,9]+273)
  }
}


#convert cm/day to cf/s by conversion and multiplying by the drainage area
met_data$ETo_cfs <- met_data$ET*(1/24)*(1/60)*(1/60)*(1/30.48)*27878000*984
met_data$PRCP_cfs <- met_data$PRCP*(1/24)*(1/60)*(1/60)*(1/12)*27878000*984


#Partition Precipitation into rain and snow accumulation
met_data$PRCP_cm <- met_data$PRCP*2.54
met_data$R <-NA
met_data$SN <- NA
met_data[1,17] <- 0
met_data$M <- NA
met_data[1,18] <- 0

for(i in seq(1:nrow(met_data))){
  if (met_data[i,9] >= 0){
    met_data[i,16] <- met_data[i, 15]
  } else{
    met_data[i,16] <- 0

}
}

for(i in seq(1:nrow(met_data))){
  if (met_data[i,9] >= 0){
    met_data[i, 18] <- met_data[i,9]*0.45
    if(met_data[i,18] > met_data[i, 17]){
      met_data[i,18] <- met_data[i,17]
      met_data[i+1,17] <- 0
    }else{
    met_data[i+1,17] <- met_data[i,17] - met_data[i,18]
    }}
  else{ 
  met_data[i+1,17] <- met_data[i, 15] + met_data[i, 17]
  met_data[i,18] <- 0
  }} 




met_data$AM5 <- rollapply(met_data$R+met_data$M, 5, sum, fill = NA, align = "right")
met_data$AM5 <- shift(met_data$AM5, n = 1, fill = NA)
met_data <- met_data[-7306,]
met_data[seq(1,5),19] <- 0


#Set CN2 to 65
CN2 <- 65

#Calculate CN1 and CN3
CN1 <- CN2/(2.334-0.01334*(CN2))
CN3 <- CN2/(0.4036+0.0059*(CN2))

#Calculate Curve Number for given day
#Run nested if/esle statment tree within a for loop
met_data$CN <- NA

for(i in seq(1,nrow(met_data))){
if(met_data[i,2] >= 5 && met_data[i,2] <=10){
  
  if(met_data[i,19] < 3.6){
    met_data[i,20] <- CN1 + (((CN2 - CN1)/(3.6-0))*met_data[i,19])
  } else if(met_data[i,19] > 3.6 && met_data[i,19] < 5.3){
    met_data[i,20] <- CN2 + (((CN3 - CN2)/(5.3 - 3.6))*((met_data[i,19])-3.6))
  }
  else {
    met_data[i,20] <- CN3}
} else {
  
  if(met_data[i,19] < 1.3){
    met_data[i,20] <- CN1 + (((CN2 - CN1)/(1.3-0))*met_data[i,19])
  }
  else if(1.3 < met_data[i,19] && met_data[i,19] < 2.8){
    met_data[i,20] <- CN2 + (((CN3 - CN2)/(2.8 - 1.3))*((met_data[i,19])-1.3))
  }
  else{
    met_data[i,20] <- CN3
  }
}  
}

for(i in seq(1,nrow(met_data))){
  if(met_data[i, 18] > 0){
    met_data[i,20] <- CN3
  }
}

#Calculate Potential Retention (S)
met_data$S <- (2540/met_data$CN)-25.4

#Calculate depth of runoff (Q), listed as (SR) from here on out
met_data$SR <- NA

for(i in seq(1,nrow(met_data))){
if(met_data[i,18]+met_data[i,16] > (0.2*met_data[i,21])){
met_data[i,22] <- (((met_data[i,18]+met_data[i,16]) - (0.2*met_data[i,21]))^2)/((met_data[i,18]+met_data[i,16]) + 0.8*met_data[i,21])
}else{
  met_data[i,22] <- 0
}
}

#Calculate Inflitration (I)
met_data$I <- met_data$R + met_data$M - met_data$SR

#Adjust ET to AET
KU <- data.frame(c(1:12), 
                 c(0.78, 0.82, 0.82, 0.79, 0.89, 0.91, 0.93, 0.98, 1.03, 0.97, 0.72, 0.61))
colnames(KU) <- c("mon", "KU")

met_data <- merge.data.frame(x = met_data, y = KU, by.x = "mon", all.y = T)
met_data<-met_data[order(met_data$DATE),]
met_data <- arrange(met_data, .by_group = met_data$DATE)   ############################################################################################### 215


met_data$AET <- met_data$ET*met_data$KU

#Calculate UNSAT conditions and adjust ET using UNSAT as an upper bound.
met_data$UNSAT <- NA
met_data[1,26] <- 10
met_data$PERC <- NA

for(i in seq(1,nrow(met_data))){
  met_data[i+1,26] <- met_data[i,26] + met_data[i,23] - met_data[i,25]
  if(met_data[i+1,26] <= 10){
    met_data[i,27] <- 0
  }else{
    met_data[i,27] <- met_data[i+1,26] - 10
    met_data[i+1,26] <- 10
  }
}

met_data <- met_data[-7306,]

#Calculate SD and SAT
met_data$SD <- NA
met_data$SAT <- NA
kb <- 0.930
met_data[1,29] <- 5.595

for(i in seq(1,nrow(met_data))){
  met_data[i,28] <- (1-kb)*met_data[i,29]
  met_data[i+1,29] <- met_data[i,29] + met_data[i,27] - met_data[i,28]
}
met_data <- met_data[-7306,]

#Calculate Q 
met_data$Q_pred <- met_data$SR + met_data$SD


####NSE and BIAS of model

#############Predicted water Statistics
#Add water year column to weather data
met_data$wyear <- as.numeric(format(met_data$DATE,'%Y')) 
is.nxt <-  as.numeric(format(met_data$DATE,'%m')) %in% 10:12 
met_data$wyear[ is.nxt ] <- met_data$wyear[is.nxt]+1 

met_data$Q_Pred_cfs <- met_data$Q_pred*(1/24)*(1/60)*(1/60)*(1/30.48)*27878000*984
#Aggregate predicted streamflow into yearly means into dataframe 
qy <-aggregate(met_data$Q_Pred_cfs ~met_data$wyear, data=met_data, FUN=mean, na.rm=TRUE)
str(qy)
colnames(qy) <- c("wyear", "mean")
qy$mean <- round(qy$mean, 0)

#Aggregate predicted streamflow data into monthly averages
qmy <- aggregate(met_data$Q_Pred_cfs~met_data$mon, FUN = mean)
colnames(qmy) <- c("mon", "mean")
qm <- aggregate(met_data$Q_Pred_cfs~met_data$mon+met_data$wyear, FUN = mean)
colnames(qm) <- c("mon", "wyear", "mean")



#################################################################################################################################


##############Actual Streamflow Values
#Add water year column to weather data
flow <- read.csv("Hydrologic_Model-SWAT-Variant/Model_Jus/4223000_streamflow.csv", header = TRUE)
flow$datetime <-as.Date(flow$datetime, format = "%m/%d/%Y")

flow$mon <- month(flow$datetime)
flow$wyear <- as.numeric(format(flow$datetime,'%Y')) 
is.nxt <-  as.numeric(format(flow$datetime,'%m')) %in% 10:12 
flow$wyear[ is.nxt ] <- flow$wyear[is.nxt]+1 

#Aggregate observed streamflow into yearly means into dataframe 
q_ac_y <-aggregate(flow$daily_mean ~flow$wyear, data=flow, FUN=mean, na.rm=TRUE)
str(q_ac_y)
colnames(q_ac_y) <- c("wyear", "mean")

#Aggregate observed streamflow data into monthly averages
q_ac_my <- aggregate(flow$daily_mean~flow$mon, FUN = mean)
q_ac_m <- aggregate(flow$daily_mean~flow$mon+flow$wyear, FUN = mean)
colnames(q_ac_my) <- c("mon", "mean")
colnames(q_ac_m) <- c("mon", "wyear", "mean")

############Bias of Annual and Monthly mean flows

obs_y <- (q_ac_y$mean)
obs_m <- (q_ac_my$mean)
pred_y <- (qy$mean)
pred_m <- (qm$mean)

obs_y_mean <- mean(q_ac_y$mean)
obs_m_mean <- mean(q_ac_my$mean)

ny <- length(q_ac_y$mean)
nm <- length(q_ac_my$mean)

#Bias for yearly average daily flow
sum3=0.0

for(i in seq(1,ny)){
  sum3 <- sum3 + (obs_y[i]-pred_y[i])/ny
}

biasyear <- signif(sum3, digits = 3)

#Bias for monthly average daily flow
sum3=0.0

for(i in seq(1,nm)){
  sum3 <- sum3 + (obs_m[i]-pred_m[i])/nm
}

biasmonth <- signif(sum3, digits = 3)

#NSE of annual average daily flow 
sum1=0.0
sum2=0.0
for(i in seq(1,ny)){
  sum1 <- sum1 + ((pred_y[i]-obs_y[i])^2)
  sum2 <- sum2 + ((obs_y[i]-obs_y_mean)^2)
  
}

NSEyear <- (1-(sum1/sum2)) 
NSEyear <- signif(NSEyear, digits = 3)

#NSE of monthly average daily flow
sum1=0.0
sum2=0.0

for(i in seq(1,nm)){
  sum1 <- sum1 + ((pred_m[i]-obs_m[i])^2)
  sum2 <- sum2 + ((obs_m[i]-obs_m_mean)^2)
  
}

NSEmonth <- (1-(sum1/sum2)) 
NSEmonth <- signif(NSEmonth, digits = 3)


######PLOTS
#Line Plot comparing observed vs. predicted daily stream flow for 1998

#Plot comparing predicted vs. observed yearly means
par(mar = (c(5,5,4,1)))
plot(q_ac_y$wyear, q_ac_y$mean, ylim = c(0, 2100), xlab = "", ylab = "", pch = 1)
points(qy$wyear, qy$mean, pch = 2) 
mtext("Water Year", line = 3, side = 1, cex = 1.2)
mtext("Average Daily Streamflow (cfs)", line = 3, side = 2, cex = 1.2)
mtext("Observed vs. Predicted Yearly Avg Daily Flow", line = 1, side = 3, cex = 1.4)
legend(legend = c("Observed", "Predicted"), pch = c(1,2), "bottomright", cex = 1)
legend(legend = c("Bias = 659", "NSE = -3.33"), x = 2002.7, y = 275, cex = 0.8)

#Plot comparing predicted vs. observed monthly means
par(mar = (c(5,5,4,1)))
plot(q_ac_my$mon, q_ac_my$mean, ylim = c(0, 3000), xlab = "", ylab = "", pch = 1)
points(qmy$mon, qmy$mean, pch = 2) 
mtext("Month", line = 3, side = 1, cex = 1.2)
mtext("Average Daily Streamflow (cfs)", line = 3, side = 2, cex = 1.2)
mtext("Observed vs. Predicted Monthly Avg Daily Flow", line = 1, side = 3, cex = 1.4)
legend(legend = c("Observed", "Predicted"), pch = c(1,2), "bottomleft", cex = 1)
legend(legend = c("Bias = 339", "NSE = -0.491"), x = 3.013, y = 400, cex = 0.8)

#############Solar Radiation ET Model
solar <- read.csv("Hydrologic_Model-SWAT-Variant/Model_Jus/radiation_data.csv", header = TRUE)
solar$DATE <- as.Date(with(solar, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

#Convert W/m2 to MJ/day
solar$GHI_J30min <- solar$GHI*1800
solarGHI <- aggregate(x = solar$GHI_J30min, by = list(Day = solar$DATE), FUN = sum)
solarGHI$GHI_MJd <- solarGHI$x/10^6

met_data_solar <- merge(met_data, solarGHI, by.x = "DATE", by.y = "Day", all = FALSE)
met_data_solar$ET_solar <- 0.018*(met_data_solar$GHI_MJd/2.45)

#Calculate UNSAT conditions and adjust ET using UNSAT as an upper bound.
met_data_solar$UNSAT <- NA
met_data_solar[1,26] <- 10
met_data_solar$PERC <- NA

for(i in seq(1,nrow(met_data_solar))){
  met_data_solar[i+1,26] <- met_data_solar[i,26] + met_data_solar[i,23] - met_data_solar[i,35]
  if(met_data_solar[i+1,26] <= 10){
    met_data_solar[i,27] <- 0
  }else{
    met_data_solar[i,27] <- met_data_solar[i+1,26] - 10
    met_data_solar[i+1,26] <- 10
  }
}

met_data_solar <- met_data_solar[-7306,]

#Calculate SD and SAT
met_data_solar$SD <- NA
met_data_solar$SAT <- NA
kb <- 0.930
met_data_solar[1,29] <- 5.595

for(i in seq(1,nrow(met_data_solar))){
  met_data_solar[i,28] <- (1-kb)*met_data_solar[i,29]
  met_data_solar[i+1,29] <- met_data_solar[i,29] + met_data_solar[i,27] - met_data_solar[i,28]
}
met_data_solar <- met_data_solar[-7306,]

#Calculate Q 
met_data_solar$Q_pred <- met_data_solar$SR + met_data_solar$SD


####NSE and BIAS of model

#############Predicted water Statistics
#Add water year column to weather data
met_data_solar$wyear <- as.numeric(format(met_data_solar$DATE,'%Y')) 
is.nxt <-  as.numeric(format(met_data_solar$DATE,'%m')) %in% 10:12 
met_data_solar$wyear[ is.nxt ] <- met_data_solar$wyear[is.nxt]+1 

met_data_solar$Q_Pred_cfs <- met_data_solar$Q_pred*(1/24)*(1/60)*(1/60)*(1/30.48)*27878000*984
#Aggregate predicted streamflow into yearly means into dataframe 
qy2 <-aggregate(met_data_solar$Q_Pred_cfs ~met_data_solar$wyear, data=met_data_solar, FUN=mean, na.rm=TRUE)
str(qy2)
colnames(qy2) <- c("wyear", "mean")
qy2$mean <- round(qy2$mean, 0)

#Aggregate predicted streamflow data into monthly averages
qmy2 <- aggregate(met_data_solar$Q_Pred_cfs~met_data_solar$mon, FUN = mean)
colnames(qmy2) <- c("mon", "mean")
qm2 <- aggregate(met_data_solar$Q_Pred_cfs~met_data_solar$mon+met_data_solar$wyear, FUN = mean)
colnames(qm2) <- c("mon", "wyear", "mean")

##############Actual Streamflow Values
#Add water year column to weather data
flow <- read.csv("Hydrologic_Model-SWAT-Variant/Model_Jus/4223000_streamflow.csv", header = TRUE)
flow$datetime <-as.Date(flow$datetime, format = "%m/%d/%Y")

flow$mon <- month(flow$datetime)
flow$wyear <- as.numeric(format(flow$datetime,'%Y')) 
is.nxt <-  as.numeric(format(flow$datetime,'%m')) %in% 10:12 
flow$wyear[ is.nxt ] <- flow$wyear[is.nxt]+1 

############Bias of Annual and Monthly mean flows
pred_y2 <- (qy2$mean)
pred_m2 <- (qm2$mean)

#Bias for yearly average daily flow
sum3=0.0

for(i in seq(1,ny)){
  sum3 <- sum3 + (obs_y[i]-pred_y2[i])/ny
}

biasyear2 <- signif(sum3, digits = 3)

#Bias for monthly average daily flow
sum3=0.0

for(i in seq(1,nm)){
  sum3 <- sum3 + (obs_m[i]-pred_m2[i])/nm
}

biasmonth2 <- signif(sum3, digits = 3)

#NSE of annual average daily flow 
sum1=0.0
sum2=0.0
for(i in seq(1,ny)){
  sum1 <- sum1 + ((pred_y2[i]-obs_y[i])^2)
  sum2 <- sum2 + ((obs_y[i]-obs_y_mean)^2)
  
}

NSEyear2 <- (1-(sum1/sum2)) 
NSEyear2 <- signif(NSEyear2, digits = 3)

#NSE of monthly average daily flow
sum1=0.0
sum2=0.0

for(i in seq(1,nm)){
  sum1 <- sum1 + ((pred_m2[i]-obs_m[i])^2)
  sum2 <- sum2 + ((obs_m[i]-obs_m_mean)^2)
  
}

NSEmonth2 <- (1-(sum1/sum2)) 
NSEmonth2 <- signif(NSEmonth2, digits = 3)


######PLOTS
#Line Plot comparing observed vs. predicted daily stream flow for 1998

#Plot comparing predicted vs. observed yearly means
par(mar = (c(5,5,4,1)))
plot(q_ac_y$wyear, q_ac_y$mean, ylim = c(-1000, 2100), xlab = "", ylab = "", pch = 1)
points(qy2$wyear, qy2$mean, pch = 16) 
points(qy$wyear, qy$mean, pch = 2) 
mtext("Water Year", line = 3, side = 1, cex = 1.2)
mtext("Average Daily Streamflow (cfs)", line = 3, side = 2, cex = 1.2)
mtext("Original vs. Alternative ET Model - Yearly Avg Daily Flow", line = 1, side = 3, cex = 1.25)
legend(legend = c("Observed", "Predicted", "Alternative"), pch = c(1,2, 16), "bottomright", cex = 1)
legend(legend = c("Original Bias = 659", "Original NSE = -3.33", "Alternative Bias = 107", "Alternative NSE = .405"), "bottomleft", cex = 0.8)

#Plot comparing predicted vs. observed monthly means
par(mar = (c(5,5,4,1)))
plot(q_ac_my$mon, q_ac_my$mean, ylim = c(-1000, 3000), xlab = "", ylab = "", pch = 1)
points(qmy2$mon, qmy2$mean, pch = 16) 
points(qmy$mon, qmy$mean, pch = 2) 
mtext("Month", line = 3, side = 1, cex = 1.2)
mtext("Average Daily Streamflow (cfs)", line = 3, side = 2, cex = 1.2)
mtext("Original vs. Alternative ET Model - Monthly Avg Daily Flow", line = 1, side = 3, cex = 1.25)
legend(legend = c("Observed", "Predicted", "Alternative"), pch = c(1,2, 16), "bottomright", cex = 1)
legend(legend = c("Original Bias = 339", "Original NSE = -0.491", "Alternative Bias = 67.7", "Alternative NSE = -2.28"), "bottomleft", cex = 0.8)



time2 <- proc.time()

time2[1]-time1[1]



plot(flow$daily_mean, met_data$Q_Pred_cfs)
