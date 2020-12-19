#Justin Beslity
#Hydrologic Modeling
#HW #2
#2/6/2019

#Purpose of script:
#Provide the following functions for the associated Beslity_HW2_Functions-Distributions.R program.

#Functions:

#mav - moving average which can be used to calculate the 7-day mean daily streamflow

mav <- function(x,n){filter(x,rep(1/n,n), sides = 1)}

#logp - Function which returns a given percentile of a dataset in a Log-Pearson Type III Distribution

lp3 <- function(x, p){
  lnx <- log(x)
  mean <- mean(lnx)
  sd <- sd(lnx)
  n <- length(x)
  g <- ((1+(6/n))*(n*sum( (lnx-mean)^3))/((n-1)*(n-2)*sd^3))
  zp <- qnorm(p)
  kp <- (2/g)*((1+((g*zp)/6)-(g^2/36))^3) - (2/g)
  Q <- exp(mean + kp*sd)
  return(Q)
}

#threep - Function which returns a given percentile of a dataset in a 3-Parameter Lognormal Distribution

threep <- function(x, p){
  xmin <- min(x)
  xmax <- max(x)
  xmed <- median(x)
  est <- (((xmin*xmax)-xmed^2)/(xmin + xmax - 2*(xmed)))
  lnx <- log(x-est)
  sd <- sd(lnx)
  mean <- mean(lnx)
  zp <- qnorm(p)
  Qthreep <- est + exp(mean + (zp*sd))
  return(Qthreep)
}

#fdcpp - Function which returns flow based on Weibull plotting position
fdcpp <- function(data, p){
  n <- nrow(data)
  i <- floor(((n+1)*p))
  theta <- ((n+1)*p - i)
  Qp <- (1-theta)*data$flow_mean[which.min(abs(data$flow_rank-i))] + theta*data$flow_mean[which.min(abs(data$flow_rank-i))]
  return(Qp)
}

