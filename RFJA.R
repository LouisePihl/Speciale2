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

#Laver ekstra datasæt med alle mulige varigheder: 
Data_alle <- Data

#Fjerner sidste datapunkt for duration
Data <- Data[Data$duration != unique(Data$duration)[length(unique(Data$duration))], ]

# Vis centreret data
View(Data)



######################################################
####                  Residualplots               ####
######################################################


residplotduration<-function(model){qplot(Data$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}


######################################################
####                  GLM MODELLER                ####
######################################################

# Lineær model med duration +konstant OE-rate
glm_int <- glm(O ~  offset(log(E)), family = poisson, data = Data)

glm_linear_1 <- glm(O ~  duration, offset = log(E), family = poisson, data = Data)

glm_linear_1_alle <- glm(O ~  duration, offset = log(E), family = poisson, data = Data_alle)

glm_poly_2_alle  <- glm(O ~  poly(duration,2), offset = log(E), family = poisson, data = Data_alle)




summary(glm_int)
summary(glm_linear_1)

AIC(glm_int,glm_linear_1)

AIC(glm_linear_1_alle, glm_poly_2_alle)

######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_linear_1_alle

#Prædiktionsplot
Data$predicted_O <- predict(model, type="response")
predplot<-function(model){Data$predicted_O <- predict(model, type="response")
Data$predicted_OE <- Data$predicted_O/Data$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(Data$duration, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(Data$duration, Data$predicted_OE, col = "red", lwd = 2)}

predplot(model)



######################################################
####    Prædictions- og residualplot DATA_alle    ####
######################################################
#Indsæt den valgte model: 
model <- glm_poly_2_alle

#Prædiktionsplot
Data_alle$predicted_O <- predict(model, type="response")
predplot<-function(model){Data_alle$predicted_O <- predict(model, type="response")
Data_alle$predicted_OE <- Data_alle$predicted_O/Data_alle$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(Data_alle$duration, Data_alle$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(Data_alle$duration, Data_alle$predicted_OE, col = "red", lwd = 2)}

predplot(model)


######################################################
####                ENDELIGE MODEL                ####
######################################################

RFJA_final <- glm(O ~  duration, offset = log(E), family = poisson, data = Data) 

summary(RFJA_final)


