#Pakker: 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)


#Regner reserve størrelse

#Definerer funktioner først
predmodel<-function(x,u){
  new_data <- data.frame(
    age = x+u,
    duration = u,
    E=1
  )
  pred<-predict(model, newdata = new_data, type = "response")
  return(as.numeric(pred))
}
predmodel_vec <- Vectorize(predmodel, vectorize.args = c("x", "u"))

trapezoidal_integration_cum <- function(x, y) {
  # Number of intervals
  n <- length(x) - 1
  # Initialize vector to store cumulative sums
  cumulative_sums <- numeric(n)
  # Initialize the integration result
  integration_result <- 0
  # Loop through each interval and calculate trapezoidal areas
  for (i in 1:n) {
    # Calculate the area of the trapezoid
    area <- (y[i] + y[i + 1]) / 2 * (x[i + 1] - x[i])
    # Add to the cumulative sum
    integration_result <- integration_result + area
    # Store the cumulative sum up to the current interval
    cumulative_sums[i] <- integration_result
  }
  return(cumulative_sums)
}
trapezoidal_integration <- function(x, y) {
  # Number of intervals
  n <- length(x) - 1
  # Initialize vector to store cumulative sums
  cumulative_sums <- numeric(n)
  # Initialize the integration result
  integration_result <- 0
  # Loop through each interval and calculate trapezoidal areas
  for (i in 1:n) {
    # Calculate the area of the trapezoid
    area <- (y[i] + y[i + 1]) / 2 * (x[i + 1] - x[i])
    # Add to the cumulative sum
    integration_result <- integration_result + area
    # Store the cumulative sum up to the current interval
    cumulative_sums[i] <- integration_result
  }
  return(integration_result)
}

mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}
mu_2p<-function(t,z){mu_21(t,z)+mu_23(t,z)}
mu2p_vec <- Vectorize(mu_2p, vectorize.args = c("t", "z"))

#Nedenstående skal ændres så det er de 4 andre intensiteter ud fra tilstanden
#SDJA<-read_csv("Data/SD/SDJA.csv")
#SDJA_OE<-sum(SDJA$O)/sum(SDJA$E)
#SDJA$OE<-SDJA$O/SDJA$E
SDRF<-read_csv("Data/SD/SDRF.csv")
SDRF_OE<-sum(SDRF$O)/sum(SDRF$E)
SDLY<-read_csv("Data/SD/SDLY.csv")
SDLY_OE<-sum(SDLY$O)/sum(SDLY$E)
SDFP<-read_csv("Data/SD/SDFP.csv")
SDFP_OE<-sum(SDFP$O)/sum(SDFP$E)
SDFJ<-read_csv("Data/SD/SDFJ.csv")
SDFJ_OE<-sum(SDFJ$O)/sum(SDFJ$E)

FJFP<-read_csv("Data/FJ/FJFP.csv")
FJFP_OE<-sum(FJFP$O)/sum(FJFP$E)
FJJA<-read_csv("Data/FJ/FJJA.csv")
FJJA_OE<-sum(FJJA$O)/sum(FJJA$E)
FJRF<-read_csv("Data/FJ/FJRF.csv")
FJRF_OE<-sum(FJRF$O)/sum(FJRF$E)
FJSD<-read_csv("Data/FJ/FJSD.csv")
FJSD_OE<-sum(FJSD$O)/sum(FJSD$E)
FJLY<-read_csv("Data/FJ/FJLY.csv")
FJLY_OE<-sum(FJLY$O)/sum(FJLY$E)


#Her kan modellen og startalder ændres ændres 
age0<-50
xseq<-seq(age0,67,0.1)
useq<-seq(0,67-age0,0.1)
tseq<-seq(0,67-age0,0.1)

model<-SDJA_final

predictions <- predmodel_vec(x = xseq, u = useq)

predictions[31:length(predictions)] <- 	SDJA_OE_endpoint #Observerede OE-rate ggregeret på alder 
#predictions[41:length(predictions)] <- 1000 #Observerede OE-rate ggregeret på alder 


predictions_mu2p<-mu2p_vec(xseq,useq)
mu_SDp<-predictions+predictions_mu2p+FJRF_OE+FJFP_OE+FJJA_OE+FJSD_OE


integrand<-exp(-trapezoidal_integration_cum(tseq,mu_SDp))
trapezoidal_integration(tseq[-1],integrand)

