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
Data <- read_csv("Data/RF/RFFJ.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

#Vis data og opsummering
view(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#Centrerer age
unique_ages <- unique(Data$age)
midpoints <- (unique_ages[-1] + unique_ages[-length(unique_ages)]) / 2
custom_last_point <- (67 - 60) / 2 + 60  # Brug 63,5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints <- c(midpoints, custom_last_point)
Data$age <- midpoints[match(Data$age, unique_ages)]

#centrerer duration
midpointsduration<-c((unique(Data$duration)[-1]+unique(Data$duration)[-length(unique(Data$duration))])/2,unique(Data$duration)[length(unique(Data$duration))])
Data$duration<-midpointsduration[match(Data$duration, unique(Data$duration))]

#fjerner sidste datapunkt for duration
Data<-Data[Data$duration!=unique(Data$duration)[length(unique(Data$duration))],]

View(Data)

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

# Sammenlign AIC for polynomial modeller for duration
AIC(glm_poly_1_dur, glm_poly_2_dur)

#residualplots
residplotduration(glm_poly_1_dur)
residplotduration(glm_poly_2_dur)

# Polynomial modeller for age
glm_poly_1_age <- glm(O ~ poly(age, 1) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_2_age <- glm(O ~ poly(age, 2) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_4_age <- glm(O ~ poly(age, 4) + duration, offset = log(E), family = poisson, data = Data)
glm_poly_6_age <- glm(O ~ poly(age, 6) + duration, offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for polynomial modeller for age
AIC(glm_poly_1_age, glm_poly_2_age, glm_poly_4_age, glm_poly_6_age)

#Residualplot
residplotage(glm_poly_2_age)
residplotage(glm_poly_4_age)
residplotage(glm_poly_6_age)

# Polynomial model med kombination af age og duration
glm_poly_6_2 <- glm(O ~ poly(age, 6) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_4_2 <- glm(O ~ poly(age, 4) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_3_2 <- glm(O ~ poly(age, 3) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_2_2 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for modeller med age og duration polynomier
AIC(glm_poly_2_2, glm_poly_3_2, glm_poly_4_2, glm_poly_6_2)


######################################################
####                   SPLINES                   ####
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
AIC(glm_poly_2_dur,glm_poly_1_age, glm_poly_2_age, glm_poly_4_age, glm_poly_6_age)

glm_splines_2_2 <- glm(O ~ ns(age, df=3) + ns(duration, df=3), offset = log(E), family = poisson, data = Data)
glm_splines_3_2 <- glm(O ~ ns(age, df=4) + ns(duration, df=3), offset = log(E), family = poisson, data = Data)
glm_splines_4_2 <- glm(O ~ ns(age, df=5) + ns(duration, df=3), offset = log(E), family = poisson, data = Data)
glm_splines_5_2 <- glm(O ~ ns(age, df=6) + ns(duration, df=3), offset = log(E), family = poisson, data = Data)

AIC(glm_splines_2_2,glm_splines_3_2,glm_splines_4_2,glm_splines_5_2)
AIC(glm_poly_2_2, glm_poly_3_2, glm_poly_4_2, glm_poly_6_2)

summary(glm_poly_2_2)
summary(glm_poly_6_2)

#Det er kun i glm_poly_2_2 at alle beta-værdier er signifikante.


######################################################
####              TILFØJER INDIKATOR              ####
######################################################

glm_ind_dur <- glm(O ~ age +poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_1 <- glm(O ~ poly(age, 2) + poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_2 <- glm(O ~ poly(age, 4) + poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = Data)

glm_ind_dur_3_3 <- glm(O ~ poly(age, 6) + poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = Data)

AIC(glm_ind_dur,glm_ind_dur_3_1,glm_ind_dur_3_2,glm_ind_dur_3_3,glm_poly_2_2)

summary(glm_ind_dur_3_1)


######################################################
####                  HEATMAPS                    ####
######################################################
#Insæt den valgte model: 
model <- glm_ind_dur_3_1

# Beregn observerede rater
Data$observed_rate <- Data$O / Data$E

# Heatmap for observerede OE-rater
heatmap_observed <- ggplot(Data, aes(y = duration, x = age, fill = observed_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Observerede OE-Rater", y = "Duration", x = "Age", fill = "Observed OE-Rate") +
  theme_minimal()

heatmap_observed


Data$predicted_O <- predict(model, type = "response")

Data$observed_predict <- Data$predicted_O/ Data$E

# Heatmap for observerede OE-rater
heatmap_predicted <- ggplot(Data, aes(y = duration, x = age, fill = observed_predict)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Observerede OE-Rater", y = "Duration", x = "Age", fill = "Observed OE-Rate") +
  theme_minimal()

heatmap_predicted


######################################################
####         Prædictions- og residualplot         ####
######################################################
#Insæt den valgte model: 
model <- glm_ind_dur_3_1

# Beregn forudsagte O-værdier
Data$predicted_O <- predict(model, type = "response")

# Vis data med forudsagte værdier
#View(Data)

# Aggreger forudsagte værdier efter age
AgeAgg <- Data %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg = sum(O), predictedAgg = sum(predicted_O)) %>%
  mutate(predicted_OE = predictedAgg / expoAgg, OE = occAgg / expoAgg)

# Plot OE-rater over alder
plot(AgeAgg$age, AgeAgg$OE, col = "blue", pch = 1, xlab = "Age", ylab = "OE Rate", 
     main = "OE-rates with additive model over Age")
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)
abline(v = 42.5, col = "steelblue", lty = 2)


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



#Insæt den valgte model: 
model <- glm_splines_2_2

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
abline(v = 42.5, col = "steelblue", lty = 2)


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



######################################################
####                ENDELIG MODEL                 ####
######################################################

RFFJ_final <- glm(O ~ poly(age, 2) + poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = Data)





