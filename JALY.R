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
Data <- read_csv("Data/JA/JALY.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

#Vis data og opsummering
view(Data)
#summary(Data)

######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#Centrerer age
unique_ages_JALY <- unique(Data$age)
midpoints_JALY <- (unique_ages_JALY[-1] + unique_ages_JALY[-length(unique_ages_JALY)]) / 2
custom_last_point_JALY <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JALY <- c(midpoints_JALY, custom_last_point_JALY)
Data$age <- midpoints_JALY[match(Data$age, unique_ages_JALY)]

#centrerer duration
midpointsduration_JALY<-c((unique(Data$duration)[-1]+unique(Data$duration)[-length(unique(Data$duration))])/2,unique(Data$duration)[length(unique(Data$duration))])
Data$duration<-midpointsduration_JALY[match(Data$duration, unique(Data$duration))]

#fjerner sidste datapunkt for duration
Data<-Data[Data$duration!=unique(Data$duration)[length(unique(Data$duration))],]

######################################################
####            VISUALISERING AF DATA             ####
######################################################
ggplot(Data, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(Data, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

plot(y=Data$O, x=Data$duration)
plot(y=Data$O, x=Data$age)



######################################################
####      AGGREGERING OG IKKE-LINEARE TRENDS      ####
######################################################

# Aggregering af data efter 'age'
AgeAgg <- Data %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Aggregering af data efter 'duration'
DurAgg <- Data %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Plots af trends
plot(AgeAgg$age, AgeAgg$OE)
plot(DurAgg$duration, DurAgg$OE)

plot(AgeAgg$age, AgeAgg$occAgg)
plot(DurAgg$duration, DurAgg$occAgg)

plot(AgeAgg$age, AgeAgg$OE)
plot(DurAgg$duration, DurAgg$OE)

plot(AgeAgg$age, AgeAgg$occAgg)
plot(DurAgg$duration, DurAgg$occAgg)

plot(AgeAgg$age, AgeAgg$expoAgg)
plot(DurAgg$duration, DurAgg$expoAgg)



#Ligner der er et skifte ved omkring to år, hvilket passer med at et forløb typisk strækker sig over to år. 



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



######################################################
####                  GLM MODELLER                ####
######################################################
glm_linear_1 <- glm(O ~ age + duration, offset = log(E), 
                    family = poisson, data = Data)

glm_linear_2 <- glm(O ~ age, offset = log(E), 
                    family = poisson, data = Data)

glm_linear_3 <- glm(O ~ duration, offset = log(E), 
                    family = poisson, data = Data)


AIC(glm_linear_1,glm_linear_2,glm_linear_3)


######################################################
####             POLYNIMIER: Duration             ####
######################################################

glm_poly_dur_1 <- glm(O ~ age + poly(duration, 1), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_dur_2 <- glm(O ~ age + poly(duration, 2), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_dur_4 <- glm(O ~ age + poly(duration, 4), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_dur_6 <- glm(O ~ age + poly(duration, 6), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_dur_8 <- glm(O ~ age + poly(duration, 8), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_dur_10 <- glm(O ~ age + poly(duration, 10), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_dur_12 <- glm(O ~ age + poly(duration, 12), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_dur_14 <- glm(O ~ age + poly(duration, 14), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_dur_16 <- glm(O ~ age + poly(duration, 16), offset = log(E), 
                   family = poisson, data = Data)

AIC(glm_poly_dur_1,glm_poly_dur_2,glm_poly_dur_4,glm_poly_dur_6,glm_poly_dur_8,glm_poly_dur_10,glm_poly_dur_12,glm_poly_dur_14,glm_poly_dur_16)

residplotduration(glm_poly_dur_2)
residplotduration(glm_poly_dur_6)
residplotduration(glm_poly_dur_10)
residplotduration(glm_poly_dur_14)
residplotduration(glm_poly_dur_16)


######################################################
####               POLYNIMIER: age                ####
######################################################

glm_poly_age_1 <- glm(O ~ poly(age, 1) + duration, offset = log(E), 
                      family = poisson, data = Data)

glm_poly_age_2 <- glm(O ~ poly(age, 2) + duration, offset = log(E), 
                      family = poisson, data = Data)

glm_poly_age_3 <- glm(O ~ poly(age, 3) + duration, offset = log(E), 
                      family = poisson, data = Data)

glm_poly_age_4 <- glm(O ~ poly(age, 4) + duration, offset = log(E), 
                      family = poisson, data = Data)

glm_poly_age_6 <- glm(O ~ poly(age, 6) + duration, offset = log(E), 
                      family = poisson, data = Data)

glm_poly_age_7 <- glm(O ~ poly(age, 7) + duration, offset = log(E), 
                      family = poisson, data = Data)

residplotage(glm_poly_age_2)
residplotage(glm_poly_age_3)
residplotage(glm_poly_age_4)
residplotage(glm_poly_age_6)
residplotage(glm_poly_age_7)
residplotduration(glm_poly_dur_14)

residplotduration(glm_poly_dur_16)

AIC(glm_poly_age_2,glm_poly_age_3,glm_poly_age_4,glm_poly_age_6,glm_poly_age_7)

######################################################
####        Prøver at fjerne sidste alder         ####
######################################################
last_age <- custom_last_point_JALY  # Dette er midtpunktet for den sidste aldersgruppe (fx 63.5)

# Lav et subset af Data, hvor du fjerner rækker, hvor age er lig med last_age (63.5)
Data_subset <- subset(Data, age != last_age)

View(Data_subset)


######################################################
####          POLYNIMIER: age SUBSET              ####
######################################################

glm_poly_age_1_subset <- glm(O ~ poly(age, 1) + duration, offset = log(E), 
                      family = poisson, data = Data_subset)

glm_poly_age_2_subset <- glm(O ~ poly(age, 2) + duration, offset = log(E), 
                      family = poisson, data = Data_subset)

glm_poly_age_3_subset <- glm(O ~ poly(age, 3) + duration, offset = log(E), 
                      family = poisson, data = Data_subset)

glm_poly_age_4_subset <- glm(O ~ poly(age, 4) + duration, offset = log(E), 
                             family = poisson, data = Data_subset)
AIC(glm_poly_age_1_subset,glm_poly_age_2_subset,glm_poly_age_3_subset,glm_poly_age_4_subset)


######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_poly_age_3_subset

Data$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- Data_subset %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)


# Plot OE-rates over age
plot(AgeAgg$age, AgeAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Age",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Age")

# Tilføj den korrekte splines kurve for OE-raterne
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)


#Tjek efter trends
DurAgg <- Data_subset %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)

# Beregn OE-rater med de korrekte E-værdier


# Plot OE-rates over age
plot(DurAgg$duration, DurAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with 16th degree polynomiall over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)

residplotduration(model)
#abline(v = my_knots, col = "green", lty = 2)





######################################################
####             SPLINES: Duration                ####
######################################################
glm_splines_2 <- glm(O ~ age + ns(duration, df = 3), offset = log(E), 
                     family = poisson, data = Data)

glm_splines_4 <- glm(O ~ age + ns(duration, df = 5), offset = log(E), 
                     family = poisson, data = Data)

glm_splines_6 <- glm(O ~ age + ns(duration, df = 7), offset = log(E), 
                     family = poisson, data = Data)

glm_splines_8 <- glm(O ~ age + ns(duration, df = 9), offset = log(E), 
                     family = poisson, data = Data)

glm_splines_10 <- glm(O ~ age + ns(duration, df = 11), offset = log(E), 
                      family = poisson, data = Data)

glm_splines_12 <- glm(O ~ age + ns(duration, df = 13), offset = log(E), 
                      family = poisson, data = Data)

glm_splines_14 <- glm(O ~ age + ns(duration, df = 15), offset = log(E), 
                      family = poisson, data = Data)

glm_splines_16 <- glm(O ~ age + ns(duration, df = 17), offset = log(E), 
                      family = poisson, data = Data)

glm_splines_18 <- glm(O ~ age + ns(duration, df = 19), offset = log(E), 
                      family = poisson, data = Data)



AIC(glm_splines_2,glm_splines_4,glm_splines_6,glm_splines_8,glm_splines_10,glm_splines_12,glm_splines_14,glm_splines_16,glm_splines_18)

######################################################
####             SPLINES: age.                    ####
######################################################
glm_splines_dur_2 <- glm(O ~ ns(age, df = 2) + duration, offset = log(E), 
                     family = poisson, data = Data)

glm_splines_dur_3 <- glm(O ~ ns(age, df = 3) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_dur_4 <- glm(O ~ ns(age, df = 4) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_dur_5 <- glm(O ~ ns(age, df = 5) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_dur_6 <- glm(O ~ ns(age, df = 6) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_dur_7 <- glm(O ~ ns(age, df = 7) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_dur_8 <- glm(O ~ ns(age, df = 8) + duration, offset = log(E), 
                         family = poisson, data = Data)



######################################################
####                  INDIKATOR                   ####
######################################################

glm_poly_dur_indi_1 <- glm(O ~ age + poly(duration, 1) + I(duration >= 2), offset = log(E), 
                           family = poisson, data = Data)


glm_poly_dur_indi_2 <- glm(O ~ age + poly(duration, 2) + I(duration >= 2), offset = log(E), 
                       family = poisson, data = Data)


glm_poly_dur_indi_3 <- glm(O ~ age + poly(duration, 3) + I(duration >= 2), offset = log(E), 
                           family = poisson, data = Data)


glm_poly_dur_indi_4 <- glm(O ~ age + poly(duration, 4) + I(duration >= 2), offset = log(E), 
                           family = poisson, data = Data)


glm_poly_dur_indi_6 <- glm(O ~ age + poly(duration, 6) + I(duration >= 2), offset = log(E), 
                           family = poisson, data = Data)

glm_poly_dur_indi_2 <- glm(O ~ poly(age,2) + poly(duration, 2):I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)

glm_poly_dur_indi_3 <- glm(O ~ poly(age,2) + poly(duration, 3):I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)

glm_poly_dur_indi_4 <- glm(O ~ poly(age,2) + poly(duration, 4):I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)

glm_poly_dur_indi_5 <- glm(O ~ poly(age,2) + poly(duration, 5):I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)

glm_poly_dur_indi_6 <- glm(O ~ poly(age,2) + poly(duration, 6)+I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)

glm_poly_dur_indi_8 <- glm(O ~ poly(age,2) + poly(duration, 8)+I(duration <= 2), offset = log(E), 
                           family = poisson, data = Data_subset)


AIC(glm_poly_dur_indi_2,glm_poly_dur_indi_3,glm_poly_dur_indi_4,glm_poly_dur_indi_5,glm_poly_dur_indi_6,glm_poly_dur_indi_8)

summary(glm_poly_dur_indi_2)

summary(glm_poly_dur_indi_5)


######################################################
####         Prædictions- og residualplot         ####
######################################################

#Indsæt den valgre model
model <- glm_poly_dur_indi_3

Data_subset$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- Data_subset %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)


# Beregn OE-rater med de korrekte E-værdier


# Plot OE-rates over age
plot(AgeAgg$age, AgeAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Age",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Age")

# Tilføj den korrekte splines kurve for OE-raterne
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)



#Tjek efter trends
DurAgg <- Data_subset %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)

# Beregn OE-rater med de korrekte E-værdier


# Plot OE-rates over age
plot(DurAgg$duration, DurAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with 2nd degree polynomiall over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)




######################################################
####               Endelige mode l                ####
######################################################

#Indsæt den valgre model
model <- glm(O ~ poly(age,2) + poly(duration, 3):I(duration <= 2), offset = log(E), 
             family = poisson, data = Data_subset)


Data_subset$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- Data_subset %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)


# Beregn OE-rater med de korrekte E-værdier


# Plot OE-rates over age
plot(AgeAgg$age, AgeAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Age",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Age")

# Tilføj den korrekte splines kurve for OE-raterne
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)



#Tjek efter trends
DurAgg <- Data_subset %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg/expoAgg, OE = occAgg/expoAgg)

# Beregn OE-rater med de korrekte E-værdier


# Plot OE-rates over age
plot(DurAgg$duration, DurAgg$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with 2nd degree polynomiall over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)







