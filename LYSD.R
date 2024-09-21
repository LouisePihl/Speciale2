######################################################
####               PAKKER OG DATA                 ####
######################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)


#Indlæs data:
Data <- read_csv("Data/LY/LYSD.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
#view(Data)
summary(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################


############# OVERVEJ DET VENSTRE ENDEPUNKT ####################

# Centrering af 'duration'
midpointsduration<-c((unique(Data$duration)[-1]+unique(Data$duration)[-length(unique(Data$duration))])/2,unique(Data$duration)[length(unique(Data$duration))])
Data$duration_centered<-midpointsduration[match(Data$duration, unique(Data$duration))]



######################################################
####            VISUALISERING AF DATA             ####
######################################################
plot(y=Data$O, x=Data$duration)
plot(y=Data$OE, x=Data$duration)


######################################################
####                  Residualplots               ####
######################################################

residplotduration<-function(model){qplot(Data$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

residplotduration(glm_linear_1)

######################################################
####                  GLM MODELLER                ####
######################################################
glm_linear_1 <- glm(O ~ duration, offset = log(E), 
                    family = poisson, data = Data)


######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_linear_1

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

residplotduration(model)
predplot(model)

######################################################
####                  POLYNIMIER                  ####
######################################################

glm_poly_1 <- glm(O ~ poly(duration, 1), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_2 <- glm(O ~ poly(duration, 2), offset = log(E), 
                  family = poisson, data = Data)

AIC(glm_linear_1,glm_poly_1,glm_poly_2)


######################################################
####                    SPLINES                   ####
######################################################
glm_splines_2 <- glm(O ~  ns(duration, df = 3), offset = log(E), 
                     family = poisson, data = Data)
glm_splines_3 <- glm(O ~  ns(duration, df = 4), offset = log(E), 
                     family = poisson, data = Data)
glm_splines_4 <- glm(O ~  ns(duration, df = 5), offset = log(E), 
                     family = poisson, data = Data)
glm_splines_5 <- glm(O ~  ns(duration, df = 5), offset = log(E), 
                     family = poisson, data = Data)
glm_splines_6 <- glm(O ~  ns(duration, df = 5), offset = log(E), 
                     family = poisson, data = Data)




######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_poly_2

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

residplotduration(model)
predplot(model)


######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_splines_2

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

residplotduration(model)
predplot(model)


AIC(glm_linear_1,glm_poly_2,glm_splines_2,glm_splines_3,glm_splines_4,glm_splines_5,glm_splines_6)






