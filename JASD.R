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
Data <- read_csv("Data/JA/JASD.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
View(Data)
summary(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

# Centrering af age
midpointsage <- c((unique(Data$age)[-1] + unique(Data$age)[-length(unique(Data$age))]) / 2, 
                  unique(Data$age)[length(unique(Data$age))])
Data$age_centered <- midpointsage[match(Data$age, unique(Data$age))]

# Centrering af duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, 
                       unique(Data$duration)[length(unique(Data$duration))])
Data$duration_centered <- midpointsduration[match(Data$duration, unique(Data$duration))]

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



######################################################
####                  GLM MODELLER                ####
######################################################

# Lineær model med age og duration
glm_linear_1 <- glm(O ~ age + duration, offset = log(E), family = poisson, data = Data)
summary(glm_linear_1)



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
glm_poly_8_age <- glm(O ~ poly(age, 8) + duration, offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for polynomial modeller for age
AIC(glm_poly_1_age, glm_poly_2_age, glm_poly_4_age, glm_poly_6_age, glm_poly_8_age)


# Polynomial model med kombination af age og duration
glm_poly_4_2 <- glm(O ~ poly(age, 4) + poly(duration, 2), offset = log(E), family = poisson, data = Data)
glm_poly_2_2 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), family = poisson, data = Data)

# Sammenlign AIC for modeller med age og duration polynomier
AIC(glm_poly_2_2, glm_poly_4_2)



######################################################
####              SPLINES DURATION                ####
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


AIC(glm_splines_2_age,glm_splines_3_age,glm_splines_4_age,glm_splines_5_age)



######################################################
####           FIT PÅ CENTRERET DATA              ####
######################################################
glm_centered_poly1age_poly3dur <- glm(O ~ poly(age_centered,1) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)

glm_centered_poly2age_poly3dur <- glm(O ~ poly(age_centered,2) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)

glm_centered_poly2age_splines2dur <- glm(O ~ poly(age_centered,2) + ns(duration,df=3), offset = log(E), 
                                         family = poisson, data = Data)

glm_centered_poly2age_splines3dur <- glm(O ~ poly(age_centered,2) + ns(duration,df=4), offset = log(E), 
                                         family = poisson, data = Data)

glm_centered_poly3age_poly3dur <- glm(O ~ poly(age_centered,3) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)

glm_centered_poly4age_poly3dur <- glm(O ~ poly(age_centered,4) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)

glm_centered_poly5age_poly3dur <- glm(O ~ poly(age_centered,5) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)

glm_centered_poly6age_poly3dur <- glm(O ~ poly(age_centered,6) + poly(duration,3), offset = log(E), 
                                      family = poisson, data = Data)


AIC(glm_centered_poly1age_poly3dur,glm_centered_poly2age_poly3dur,glm_centered_poly3age_poly3dur,glm_centered_poly4age_poly3dur,glm_centered_poly5age_poly3dur,glm_centered_poly6age_poly3dur,glm_centered_poly2age_splines2dur,glm_centered_poly2age_splines3dur)



######################################################
####         Prædictions- og residualplot         ####
######################################################

#Insæt den valgte model: 
model <- glm_linear_1

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


# Residualplot for lineær model
residplotage(model)
residplotduration(model)



######################################################
####              ENDELIGE MODEL                  ####
######################################################


