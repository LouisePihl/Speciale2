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
Data <- read_csv("Data/FP/FPFJ.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
#View(Data)
summary(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

# Centrering af age
midpointsage <- c((unique(Data$age)[-1] + unique(Data$age)[-length(unique(Data$age))]) / 2, 
                  unique(Data$age)[length(unique(Data$age))])
Data$age <- midpointsage[match(Data$age, unique(Data$age))]

#centrerer duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration <- midpointsduration[match(Data$duration, unique(Data$duration))]

#fjerner sidste datapunkt for duration
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

# Sammenlign AIC for polynomial modeller for duration
AIC(glm_poly_1_dur, glm_poly_2_dur)

# Polynomial modeller for age
glm_poly_1_age <- glm(O ~ poly(age, 1) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_2_age <- glm(O ~ poly(age, 2) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_4_age <- glm(O ~ poly(age, 4) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_6_age <- glm(O ~ poly(age, 6) + duration, offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for polynomial modeller for age
AIC(glm_poly_1_age, glm_poly_2_age, glm_poly_4_age, glm_poly_6_age)


# Polynomial model med kombination af age og duration
glm_poly_6_2 <- glm(O ~ poly(age, 6) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_4_2 <- glm(O ~ poly(age, 4) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_3_2 <- glm(O ~ poly(age, 3) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_2_2 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for modeller med age og duration polynomier
AIC(glm_poly_2_2, glm_poly_3_2, glm_poly_4_2, glm_poly_6_2)



######################################################
####              SPLINES AGE + DUR               ####
######################################################
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


#Sammenlignes med polynomier: 
AIC(glm_splines_5_age,glm_poly_6_age,glm_poly_4_2)




######################################################
####         Prædictions- og residualplot         ####
######################################################
#Insæt den valgte model: 
model <- glm_poly_4_2

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
abline(v = 32.5, col = "steelblue", lty = 2)


# Aggreger forudsagte værdier efter duration
DurAgg <- Data %>%
  group_by(duration) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg / expoAgg, OE = occAgg / expoAgg)

# Plot OE-rater over duration
plot(DurAgg$duration, DurAgg$OE, col = "blue", pch = 1, xlab = "Duration", ylab = "OE Rate", 
     main = "OE-rates with additive model over Duration")
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)


# Residualplot for lineær model
residplotage(model)
residplotduration(model)


######################################################
####               ENDELIGE MODEL                 ####
######################################################

FPFJ_final <- glm(O ~ poly(age, 4) + poly(duration, 2), offset = log(E), family = poisson, data = Data)

summary(FPFJ_final)

AIC(FPFJ_final)








