######################################################
####               PAKKER OG DATA                 ####
######################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

# Indlæs data:
Data <- read_csv("Data/RF/RFJA.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
View(Data)
summary(Data)


######################################################
####            VISUALISERING AF DATA             ####
######################################################

# Plot O mod duration
plot(y = Data$O, x = Data$duration)

# Plot E mod duration
plot(y = Data$E, x = Data$duration)

# Plot OE mod duration
plot(y = Data$OE, x = Data$duration)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#Centrerer duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration_center <- midpointsduration[match(Data$duration, unique(Data$duration))]

View(Data_subset)

# Vis centreret data
View(Data)




