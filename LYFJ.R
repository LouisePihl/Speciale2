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
Data <- read_csv("Data/LY/LYFJ.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
View(Data)
summary(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#Centrerer age
unique_ages <- unique(Data$age)
midpoints <- (unique_ages[-1] + unique_ages[-length(unique_ages)]) / 2
custom_last_point <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints <- c(midpoints, custom_last_point)
Data$age <- midpoints[match(Data$age, unique_ages)]

#Centrerer duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration <- midpointsduration[match(Data$duration, unique(Data$duration))]

#Fjerner sidste datapunkt for duration
Data <- Data[Data$duration != unique(Data$duration)[length(unique(Data$duration))], ]

# Vis centreret data
View(Data)

######################################################
####            VISUALISERING AF DATA             ####
######################################################

# Plot O mod duration
plot(y = Data$O, x = Data$duration)

# Plot O mod age
plot(y = Data$O, x = Data$age)


# Scatter plot af OE-rater med alder og duration
ggplot(Data, aes(x = age, y = OE)) +
  geom_point(aes(color = duration)) +
  facet_wrap(~ duration)

# Scatter plot af OE-rater med duration og alder
ggplot(Data, aes(x = duration, y = OE)) +
  geom_point(aes(color = age)) +
  facet_wrap(~ age)


######################################################
####                  HEATMAPS                    ####
######################################################

# Beregn observerede rater
Data$observed_rate <- Data$O / Data$E

# Heatmap for observerede OE-rater
heatmap_observed <- ggplot(Data, aes(y = duration, x = age, fill = observed_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Observerede OE-Rater", y = "Duration", x = "Age", fill = "Observed OE-Rate") +
  theme_minimal()

heatmap_observed

# Heatmap for centrerede observerede OE-rater
heatmap_observed_center <- ggplot(Data, aes(y = duration_centered, x = age_centered, fill = observed_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Centerede Observerede OE-Rater", y = "Duration", x = "Age", fill = "Observed OE-Rate") +
  theme_minimal()

heatmap_observed_center

######################################################
####      AGGREGERING OG IKKE-LINEARE TRENDS      ####
######################################################

# Aggreger data efter age
AgeAgg <- Data %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O)) %>%
  mutate(OE = occAgg / expoAgg)

# Aggreger data efter duration
DurAgg <- Data %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O)) %>%
  mutate(OE = occAgg / expoAgg)

# Plots af aggregerede data
plot(AgeAgg$age, AgeAgg$occAgg)
plot(DurAgg$duration, DurAgg$occAgg)

plot(AgeAgg$age, AgeAgg$OE)
plot(DurAgg$duration, DurAgg$OE)

plot(AgeAgg$age, AgeAgg$expoAgg)
plot(DurAgg$duration, DurAgg$expoAgg)


######################################################
####                  Residualplots               ####
######################################################


#Centrer middelværdien: 
residplotage<-function(model){qplot(Data$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(Data$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}







