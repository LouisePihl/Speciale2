######################################################
####               PAKKER OG DATA                 ####
######################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

######################################################
####              DATASÆT TIL FP                  ####
######################################################

SDFP<-read_csv("Data/SD/SDFP.csv")
SDFP_OE<-sum(SDFP$O)/sum(SDFP$E)


JAFP<-read_csv("Data/JA/JAFP.csv")
JAFP_OE<-sum(JAFP$O)/sum(JAFP$E)


RFFP<-read_csv("Data/RF/RFFP.csv")
RFFP_OE<-sum(RFFP$O)/sum(RFFP$E)


LYFP<-read_csv("Data/LY/LYFP.csv")
LYFP_OE<-sum(LYFP$O)/sum(LYFP$E) #KUN ALDER
LYFP$OE <- LYFP$O / LYFP$E


FJFP<-read_csv("Data/FJ/FJFP.csv")
FJFP_OE<-sum(FJFP$O)/sum(FJFP$E)


######################################################
####      AGGREGERING OG IKKE-LINEARE TRENDS      ####
######################################################

# Aggregering af data efter 'age'
AgeAgg_SDFP <- SDFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_JAFP <- JAFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_RFFP <- RFFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_FJFP <- FJFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_LYFP <- LYFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Plots af trends
plot(AgeAgg_SDFP$age, AgeAgg_SDFP$OE)
plot(AgeAgg_JAFP$age, AgeAgg_JAFP$OE)
plot(AgeAgg_RFFP$age, AgeAgg_RFFP$OE)
plot(LYFP$age, LYFP$OE)
plot(AgeAgg_FJFP$age, AgeAgg_FJFP$OE)


# Opret første plot med linjer
plot(AgeAgg_SDFP$age, AgeAgg_SDFP$OE, type = "l", col = "blue", lwd = 2.5, 
     xlab = "Age", ylab = "OE", main = "OE-rates for different age groups", ylim=c(0,0.09), xlim=c(18,70))

# Tilføj yderligere linjer til plottet
lines(AgeAgg_JAFP$age, AgeAgg_JAFP$OE, col = "aquamarine3", lwd = 2.5)
lines(AgeAgg_RFFP$age, AgeAgg_RFFP$OE, col = "forestgreen", lwd = 2.5)
lines(AgeAgg_LYFP$age, AgeAgg_LYFP$OE, col = "steelblue", lwd = 2.5)
lines(AgeAgg_FJFP$age, AgeAgg_FJFP$OE, col = "darkseagreen4", lwd = 2.5)

# Tilføj en legende for at identificere de forskellige grupper
legend("topleft", legend = c("SDFP", "JAFP", "RFFP", "LYFP", "FJFP"), 
       col = c("blue", "aquamarine3", "forestgreen", "steelblue", "darkseagreen4"), lwd = 3)
abline(v = 40, col = "black", lty = 2)
