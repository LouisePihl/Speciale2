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
Data <- read_csv("Data/LY/LYJA.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
View(Data)
summary(Data)

Data$OE #0.000264376



######################################################
####               ENDELIGE MODEL                 ####
######################################################

LYJA_final <- 0.000264376

