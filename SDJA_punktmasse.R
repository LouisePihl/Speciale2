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
SDJA <- read_csv("Data/SD/SDJA.csv")

# Opret variabel OE-rater
SDJA$OE <- SDJA$O / SDJA$E

# Vis data og opsummering
#view(Data)
#summary(Data)


######################################################
####        Datapunkt ved u = 2 måneder           ####
######################################################

#OE-raten for varrighed 2 måneder for alle aldre
SDJA_filtered <- SDJA[abs(SDJA$duration - 0.16666667) < 1e-8, ]
SDJA_OE_2m<-sum(SDJA_filtered$O)/sum(SDJA_filtered$E)

#Centrerer age
unique_ages_SDJA <- unique(SDJA$age)
midpoints_SDJA <- (unique_ages_SDJA[-1] + unique_ages_SDJA[-length(unique_ages_SDJA)]) / 2
custom_last_point_SDJA <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDJA <- c(midpoints_SDJA, custom_last_point_SDJA)
SDJA$age <- midpoints_SDJA[match(SDJA$age, unique_ages_SDJA)]


#Centrerer duration
midpointsduration_SDJA<-c((unique(SDJA$duration)[-1]+unique(SDJA$duration)[-length(unique(SDJA$duration))])/2,unique(SDJA$duration)[length(unique(SDJA$duration))])
SDJA$duration<-midpointsduration_SDJA[match(SDJA$duration, unique(SDJA$duration))]

#Fjerner punktet ved 3 år
SDJA<-SDJA[SDJA$duration!=unique(SDJA$duration)[length(unique(SDJA$duration))],]
#Fjerner punktet ved 2 måneder
SDJA <- SDJA[SDJA$duration != unique(SDJA$duration)[3], ]



print(midpointsduration_SDJA)
print(unique(SDJA$duration))

view(SDJA)

######################################################
####           Punktmasse: q-værdi                ####
######################################################

q <- SDJA_OE_2m


######################################################
####                  Ny model                    ####
######################################################

#----------------VISUALISERING AF DATA---------------
ggplot(SDJA, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(SDJA, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

plot(y=SDJA$O, x=SDJA$duration)
plot(y=SDJA$O, x=SDJA$age)


#---------AGGREGERING OG IKKE-LINEARE TRENDS--------

# Aggregering af data efter 'age'
AgeAgg <- SDJA %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Aggregering af data efter 'duration'
DurAgg <- SDJA %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Plots af trends
plot(AgeAgg$age, AgeAgg$OE,xlab="Age aggregated", ylab="OE rates")
plot(DurAgg$duration, DurAgg$OE,xlab="Duration aggregated", ylab="OE rates")

plot(AgeAgg$age, AgeAgg$occAgg,xlab="Age aggregated", ylab="Occurance")
plot(DurAgg$duration, DurAgg$occAgg,xlab="Duration aggregated", ylab="Occurance")

plot(AgeAgg$age, AgeAgg$occAgg)
plot(DurAgg$duration, DurAgg$occAgg)

plot(AgeAgg$age, AgeAgg$expoAgg,xlab="Age aggregated", ylab="Exposure")
plot(DurAgg$duration, DurAgg$expoAgg,xlab="Duration aggregated", ylab="Exposure")




######################################################
####                  Residualplots               ####
######################################################


residplotage<-function(model){qplot(SDJA$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("Age") +
    ylab("Fitted values")}

residplotduration<-function(model){qplot(SDJA$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("Duration") +
    ylab("Fitted values")}


######################################################
####                  GLM MODELLER                ####
######################################################
glm_linear_1 <- glm(O ~ age + duration, offset = log(E), 
                    family = poisson, data = SDJA)

glm_linear_2 <- glm(O ~ age, offset = log(E), 
                    family = poisson, data = SDJA)

glm_linear_3 <- glm(O ~ duration, offset = log(E), 
                    family = poisson, data = SDJA)


AIC(glm_linear_1,glm_linear_2,glm_linear_3)


######################################################
####                  POLYNIMIER                  ####
######################################################

glm_poly_1 <- glm(O ~ age + poly(duration, 1), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_2 <- glm(O ~ age + poly(duration, 2), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_4 <- glm(O ~ age + poly(duration, 4), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_6 <- glm(O ~ age + poly(duration, 6), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_8 <- glm(O ~ age + poly(duration, 8), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_10 <- glm(O ~ age + poly(duration, 10), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_12 <- glm(O ~ age + poly(duration, 12), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_14 <- glm(O ~ age + poly(duration, 14), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_16 <- glm(O ~ age + poly(duration, 16), offset = log(E), 
                   family = poisson, data =SDJA)

glm_poly_18 <- glm(O ~ age + poly(duration, 18), offset = log(E), 
                   family = poisson, data = SDJA)


AIC(glm_poly_1,glm_poly_2,glm_poly_4,glm_poly_6,glm_poly_8,glm_poly_10,glm_poly_12,glm_poly_14,glm_poly_16,glm_poly_18)

residplotduration(glm_poly_2)
residplotduration(glm_poly_6)
residplotduration(glm_poly_10)
residplotduration(glm_poly_16)
residplotduration(glm_poly_18)



glm_poly_2_1 <- glm(O ~ poly(age, 2) + poly(duration, 1), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_2_2 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), 
                  family = poisson, data = SDJA)


glm_poly_2_4 <- glm(O ~ poly(age, 2) + poly(duration, 4), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_2_6 <- glm(O ~ poly(age, 2) + poly(duration, 6), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_2_8 <- glm(O ~ poly(age, 2) + poly(duration, 8), offset = log(E), 
                  family = poisson, data = SDJA)

glm_poly_2_10 <- glm(O ~ poly(age, 2) + poly(duration, 10), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_2_12 <- glm(O ~ poly(age, 2) + poly(duration, 12), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_2_14 <- glm(O ~ poly(age, 2) + poly(duration, 14), offset = log(E), 
                   family = poisson, data = SDJA)

glm_poly_2_16 <- glm(O ~ poly(age, 2) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = SDJA)

glm_poly_2_18 <- glm(O ~ poly(age, 2) + poly(duration, 18), offset = log(E), 
                   family = poisson, data = SDJA)

AIC(glm_poly_2_1,glm_poly_2_2,glm_poly_2_4,glm_poly_2_6,glm_poly_2_8,glm_poly_2_10,glm_poly_2_12,glm_poly_2_14,glm_poly_2_16,glm_poly_2_18)



######################################################
####                  1 INDIKATOR                 ####
######################################################
glm_2indikator_add <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                          family = poisson, data = SDJA)

glm_indikator_add <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12), offset = log(E), 
                         family = poisson, data = SDJA)

AIC(glm_2indikator_add, glm_indikator_add)




#Lav en indikater og tjek samtlige polynomier grader:
glm_indikator_poly1 <- glm(O ~ poly(age, 2) + poly(duration, 1) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)

glm_indikator_poly2 <- glm(O ~ poly(age, 2) + poly(duration, 2) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)

glm_indikator_poly4 <- glm(O ~ poly(age, 2) + poly(duration, 4) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)

glm_indikator_poly6 <- glm(O ~ poly(age, 2) + poly(duration, 6) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)

glm_indikator_poly8 <- glm(O ~ poly(age, 2) + poly(duration, 8) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)

glm_indikator_poly10 <- glm(O ~ poly(age, 2) + poly(duration, 10) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = SDJA)

glm_indikator_poly12 <- glm(O ~ poly(age, 2) + poly(duration, 12) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = SDJA)

glm_indikator_poly14 <- glm(O ~ poly(age, 2) + poly(duration, 14) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = SDJA)

glm_indikator_poly16 <- glm(O ~ poly(age, 2) + poly(duration, 16) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = SDJA)

glm_indikator_poly18 <- glm(O ~ poly(age, 2) + poly(duration, 18) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = SDJA)

AIC(glm_indikator_poly1,glm_indikator_poly2,glm_indikator_poly4,glm_indikator_poly6,glm_indikator_poly8,glm_indikator_poly10,glm_indikator_poly12,glm_indikator_poly14,glm_indikator_poly16,glm_indikator_poly18)



######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_indikator_poly4 

SDJA$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- SDJA %>%
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
DurAgg <- SDJA %>%
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
     main = "OE-rates with additive model over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)

residplotage(model)
residplotduration(model)
#abline(v = my_knots, col = "green", lty = 2)


######################################################
####       Endelige nye model uden punktmasse     ####
######################################################
SDJA_punktmasse_model <- glm(O ~ poly(age, 2) + poly(duration, 4) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = SDJA)


