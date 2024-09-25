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
Data <- read_csv("Data/RF/RFFP.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
View(Data)
summary(Data)

######################################################
####                  KOMMENTAR                   ####
######################################################

#Sletter første punkt 18-40 for nu og håndterer det efterfølgende


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#Centrerer age
unique_ages <- unique(Data$age)
midpoints <- (unique_ages[-1] + unique_ages[-length(unique_ages)]) / 2
custom_last_point <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints <- c(midpoints, custom_last_point)
Data$age <- midpoints[match(Data$age, unique_ages)]

#Fjerner første datapunkt for age
Data <- Data[Data$age != unique(Data$age)[1], ]

#Centrerer duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration <- midpointsduration[match(Data$duration, unique(Data$duration))]

#Fjerner sidste datapunkt for duration: 3.5
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



######################################################
####                  GLM MODELLER                ####
######################################################

# Lineær model med age og duration
glm_linear_1 <- glm(O ~ age + duration, offset = log(E), family = poisson, data = Data)

summary(glm_linear_1)
AIC(glm_linear_1)


######################################################
####                 POLYNOMIUM                   ####
######################################################

# Polynomial modeller for duration
glm_poly_1_dur <- glm(O ~ age + poly(duration, 1), offset = log(E), family = poisson, data = Data)
glm_poly_2_dur <- glm(O ~ age + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_3_dur <- glm(O ~ age + poly(duration, 3), offset = log(E), family = poisson, data = Data)
glm_poly_4_dur <- glm(O ~ age + poly(duration, 4), offset = log(E), family = poisson, data = Data)
glm_poly_5_dur <- glm(O ~ age + poly(duration, 5), offset = log(E), family = poisson, data = Data)

residplotduration(glm_poly_3_dur)
residplotduration(glm_poly_4_dur)
residplotduration(glm_poly_5_dur)

# Sammenlign AIC for polynomial modeller for duration
AIC(glm_poly_1_dur, glm_poly_2_dur,glm_poly_3_dur,glm_poly_4_dur,glm_poly_5_dur)

# Polynomial modeller for age
glm_poly_1_age <- glm(O ~ poly(age, 1) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_2_age <- glm(O ~ poly(age, 2) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_3_age <- glm(O ~ poly(age, 3) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_4_age <- glm(O ~ poly(age, 4) + duration, offset = log(E), family = poisson, data = Data)

residplotage(glm_poly_2_age)
residplotage(glm_poly_3_age)
residplotage(glm_poly_4_age)

# Sammenlign AIC for polynomial modeller for age
AIC(glm_poly_1_age, glm_poly_2_age, glm_poly_4_age)


# Polynomial model med kombination af 2. grads poly på age og forskellige duration
glm_poly_2_6 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_2_4 <- glm(O ~ poly(age, 2) + poly(duration, 3), offset = log(E), family = poisson, data = Data)
glm_poly_2_3 <- glm(O ~ poly(age, 2) + poly(duration, 4), offset = log(E), family = poisson, data = Data)
glm_poly_2_2 <- glm(O ~ poly(age, 2) + poly(duration, 5), offset = log(E), family = poisson, data = Data)

# Polynomial model med kombination af 3. grads poly på age og forskellige duration
glm_poly_3_6 <- glm(O ~ poly(age, 3) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_3_4 <- glm(O ~ poly(age, 3) + poly(duration, 3), offset = log(E), family = poisson, data = Data)
glm_poly_3_3 <- glm(O ~ poly(age, 3) + poly(duration, 4), offset = log(E), family = poisson, data = Data)
glm_poly_3_2 <- glm(O ~ poly(age, 3) + poly(duration, 5), offset = log(E), family = poisson, data = Data)


# Sammenlign AIC for modeller med age og duration polynomier
AIC(glm_poly_2_2, glm_poly_2_3, glm_poly_2_4, glm_poly_2_6)
AIC(glm_poly_3_2, glm_poly_3_3, glm_poly_3_4, glm_poly_3_6)

#Bedste AIC på 3x3 poly

residplotage(glm_poly_3_3)
residplotduration(glm_poly_3_3)


######################################################
####              TILFØJER INDIKATOR              ####
######################################################

glm_ind_dur <- glm(O ~ age + duration + I(duration <= 3), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_1 <- glm(O ~ poly(age, 3) + poly(duration, 1) + I(duration <= 3), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_2 <- glm(O ~ poly(age, 3) + poly(duration, 2) + I(duration <= 3), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_3 <- glm(O ~ poly(age, 3) + poly(duration, 3) + I(duration <= 3), offset = log(E), family = poisson, data = Data)

AIC(glm_ind_dur,glm_ind_dur_3_1,glm_ind_dur_3_2,glm_ind_dur_3_3,glm_poly_3_3)

summary(glm_ind_dur_3_2)

######################################################
####              SPLINES AGE + DUR               ####
######################################################

#Splines på covariabler seperat:
glm_splines_2_dur <- glm(O ~ ns(duration, df = 3) + age, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_3_dur <- glm(O ~ ns(duration, df = 4) + age, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_2_age <- glm(O ~ ns(age, df = 3) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_3_age <- glm(O ~ ns(age, df = 4) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_4_age <- glm(O ~ ns(age, df = 5) + duration, offset = log(E), 
                         family = poisson, data = Data)

glm_splines_5_age <- glm(O ~ ns(age, df = 6) + duration, offset = log(E), 
                         family = poisson, data = Data)


AIC(glm_splines_2_dur,glm_splines_3_dur,glm_splines_2_age,glm_splines_3_age,glm_splines_4_age,glm_splines_5_age)


#Splines på begge covariable:
#Holder age fast:
glm_splines_2_2 <- glm(O ~ ns(age, df = 3) + ns(duration, df = 3), offset = log(E), 
                         family = poisson, data = Data)

glm_splines_2_3 <- glm(O ~ ns(age, df = 3) + ns(duration, df = 4), offset = log(E), 
                         family = poisson, data = Data)

glm_splines_2_4 <- glm(O ~ ns(age, df = 3) + ns(duration, df = 5), offset = log(E), 
                       family = poisson, data = Data)

glm_splines_2_5 <- glm(O ~ ns(age, df = 3) + ns(duration, df = 6), offset = log(E), 
                       family = poisson, data = Data)

AIC(glm_splines_2_2,glm_splines_2_3,glm_splines_2_4,glm_splines_2_5)



glm_splines_3_2 <- glm(O ~ ns(age, df = 4) + ns(duration, df = 3), offset = log(E), 
                       family = poisson, data = Data)

glm_splines_3_3 <- glm(O ~ ns(age, df = 4) + ns(duration, df = 4), offset = log(E), 
                       family = poisson, data = Data)

glm_splines_3_4 <- glm(O ~ ns(age, df = 4) + ns(duration, df = 5), offset = log(E), 
                       family = poisson, data = Data)

glm_splines_3_5 <- glm(O ~ ns(age, df = 4) + ns(duration, df = 6), offset = log(E), 
                       family = poisson, data = Data)

AIC(glm_splines_3_2,glm_splines_3_3,glm_splines_3_4,glm_splines_3_5)

#AIC'en er bedre for age,df=3


#Holder duration fast:
glm_splines_2_2 <- glm(O ~ ns(age, df = 3) + ns(duration, df = 3), offset = log(E), 
                         family = poisson, data = Data)

glm_splines_3_2 <- glm(O ~ ns(age, df = 4) + ns(duration, df = 3), offset = log(E), 
                         family = poisson, data = Data)

glm_splines_4_2 <- glm(O ~ ns(age, df = 5) + ns(duration, df = 3), offset = log(E), 
                         family = poisson, data = Data)

AIC(glm_splines_2_2,glm_splines_3_2,glm_splines_4_2)

#Sammenligner mwed poly, med og uden indikator: 
AIC(glm_splines_2_2,glm_ind_dur_3_3,glm_poly_3_3)


######################################################
####         Prædictions- og residualplot         ####
######################################################
#Insæt den valgte model: 
model <- glm_ind_dur_3_2

# Beregn forudsagte O-værdier
Data$predicted_O <- predict(model, type = "response")

# Vis data med forudsagte værdier
View(Data)

# Aggreger forudsagte værdier efter age
AgeAgg <- Data %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg / expoAgg, OE = occAgg / expoAgg)

# Plot OE-rater over alder
plot(AgeAgg$age, AgeAgg$OE, col = "blue", pch = 1, xlab = "Age", ylab = "OE Rate", 
     main = "OE-rates with additive model over Age")
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)


# Aggreger forudsagte værdier efter duration
DurAgg <- Data %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg / expoAgg, OE = occAgg / expoAgg)

# Plot OE-rater over duration
plot(DurAgg$duration, DurAgg$OE, col = "blue", pch = 1, xlab = "Duration", ylab = "OE Rate", 
     main = "OE-rates with additive model over Duration")
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)
abline(v = 3, col = "steelblue", lty = 2)


# Residualplot for lineær model
residplotage(model)
residplotduration(model)


######################################################
####               ENDELIGE MODEL                 ####
######################################################

RFFP_final <- glm(O ~ poly(age, 3) + poly(duration, 2) + I(duration <= 3), offset = log(E), family = poisson, data = Data)

summary(RFFP_final)

AIC(RFFP_final)





