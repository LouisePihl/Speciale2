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
Data <- read_csv("Data/SD/SDJA.csv")

# Opret variabel OE-rater
Data$OE <- Data$O / Data$E

# Vis data og opsummering
view(Data)
summary(Data)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

# Centrering af 'age'
midpointsage<-c((unique(Data$age)[-1]+unique(Data$age)[-length(unique(Data$age))])/2,unique(Data$age)[length(unique(Data$age))])
Data$age_centered<-midpointsage[match(Data$age, unique(Data$age))]

# Centrering af 'duration'
midpointsduration<-c((unique(Data$duration)[-1]+unique(Data$duration)[-length(unique(Data$duration))])/2,unique(Data$duration)[length(unique(Data$duration))])
Data$duration_centered<-midpointsduration[match(Data$duration, unique(Data$duration))]

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
glm_linear_1 <- glm(O ~ age + duration, offset = log(E), 
                    family = poisson, data = Data)

glm_linear_2 <- glm(O ~ age, offset = log(E), 
                    family = poisson, data = Data)

glm_linear_3 <- glm(O ~ duration, offset = log(E), 
                    family = poisson, data = Data)





######################################################
####                  POLYNIMIER                  ####
######################################################

glm_poly_1 <- glm(O ~ age + poly(duration, 1), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_2 <- glm(O ~ age + poly(duration, 2), offset = log(E), 
                  family = poisson, data = Data)


glm_poly_4 <- glm(O ~ age + poly(duration, 4), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_6 <- glm(O ~ age + poly(duration, 6), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_8 <- glm(O ~ age + poly(duration, 8), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_10 <- glm(O ~ age + poly(duration, 10), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_12 <- glm(O ~ age + poly(duration, 12), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_14 <- glm(O ~ age + poly(duration, 14), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_16 <- glm(O ~ age + poly(duration, 16), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_18 <- glm(O ~ age + poly(duration, 18), offset = log(E), 
                   family = poisson, data = Data)


AIC(glm_poly_1,glm_poly_2,glm_poly_4,glm_poly_6,glm_poly_8,glm_poly_10,glm_poly_12,glm_poly_14,glm_poly_16,glm_poly_18)

residplotduration(glm_poly_2)
residplotduration(glm_poly_6)
residplotduration(glm_poly_10)
residplotduration(glm_poly_16)
residplotduration(glm_poly_18)



glm_poly_1 <- glm(O ~ poly(age, 2) + poly(duration, 1), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_2 <- glm(O ~ poly(age, 2) + poly(duration, 2), offset = log(E), 
                  family = poisson, data = Data)


glm_poly_4 <- glm(O ~ poly(age, 2) + poly(duration, 4), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_6 <- glm(O ~ poly(age, 2) + poly(duration, 6), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_8 <- glm(O ~ poly(age, 2) + poly(duration, 8), offset = log(E), 
                  family = poisson, data = Data)

glm_poly_10 <- glm(O ~ poly(age, 2) + poly(duration, 10), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_12 <- glm(O ~ poly(age, 2) + poly(duration, 12), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_14 <- glm(O ~ poly(age, 2) + poly(duration, 14), offset = log(E), 
                   family = poisson, data = Data)

glm_poly_2_16 <- glm(O ~ poly(age, 2) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = Data)

glm_poly_18 <- glm(O ~ poly(age, 2) + poly(duration, 18), offset = log(E), 
                   family = poisson, data = Data)

AIC(glm_poly_1,glm_poly_2,glm_poly_4,glm_poly_6,glm_poly_8,glm_poly_10,glm_poly_12,glm_poly_14,glm_poly_16,glm_poly_18)

summary(glm_poly_16)


glm_poly_2_16 <- glm(O ~ poly(age, 2) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = Data)
glm_poly_4_16 <- glm(O ~ poly(age, 4) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = Data)
glm_poly_6_16 <- glm(O ~ poly(age, 6) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = Data)
glm_poly_8_16 <- glm(O ~ poly(age, 8) + poly(duration, 16), offset = log(E), 
                     family = poisson, data = Data)
glm_poly_10_16 <- glm(O ~ poly(age, 10) + poly(duration, 16), offset = log(E), 
                      family = poisson, data = Data)
glm_poly_12_16 <- glm(O ~ poly(age, 12) + poly(duration, 16), offset = log(E), 
                      family = poisson, data = Data)


glm_poly_2_1 <- glm(O ~ poly(age, 2) + poly(duration, 1), offset = log(E), 
                    family = poisson, data = Data)
glm_poly_4_1 <- glm(O ~ poly(age, 4) + poly(duration, 1), offset = log(E), 
                    family = poisson, data = Data)
glm_poly_6_1 <- glm(O ~ poly(age, 6) + poly(duration, 1), offset = log(E), 
                    family = poisson, data = Data)
glm_poly_8_1 <- glm(O ~ poly(age, 8) + poly(duration, 1), offset = log(E), 
                    family = poisson, data = Data)
glm_poly_10_1 <- glm(O ~ poly(age, 10) + poly(duration, 1), offset = log(E), 
                     family = poisson, data = Data)
glm_poly_12_1 <- glm(O ~ poly(age, 12) + poly(duration, 1), offset = log(E), 
                     family = poisson, data = Data)


AIC(glm_poly_2_16,glm_poly_4_16,glm_poly_6_16,glm_poly_8_16,glm_poly_10_16,glm_poly_12_16)
AIC(glm_poly_2_1,glm_poly_4_1,glm_poly_6_1,glm_poly_8_1,glm_poly_10_1,glm_poly_12_1)



######################################################
####         Prædictions- og residualplot         ####
######################################################
#Indsæt den valgte model: 
model <- glm_poly_16

Data$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- Data %>%
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
DurAgg <- Data %>%
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
####                    SPLINES                   ####
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
####                  1 INDIKATOR                 ####
######################################################
glm_2indikator_add <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                          family = poisson, data = Data)

glm_indikator_add <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12), offset = log(E), 
                         family = poisson, data = Data)

AIC(glm_2indikator_add, glm_indikator_add)




#Lav en indikater og tjek samtlige polynomier grader:
glm_indikator_poly1 <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = Data)
glm_indikator_poly2 <- glm(O ~ age + poly(duration, 2) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = Data)
glm_indikator_poly4 <- glm(O ~ age + poly(duration, 4) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = Data)
glm_indikator_poly6 <- glm(O ~ age + poly(duration, 6) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = Data)
glm_indikator_poly8 <- glm(O ~ age + poly(duration, 8) + I(duration >= 2/12), offset = log(E), 
                           family = poisson, data = Data)
glm_indikator_poly10 <- glm(O ~ age + poly(duration, 10) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = Data)

glm_indikator_poly12 <- glm(O ~ age + poly(duration, 12) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = Data)

glm_indikator_poly14 <- glm(O ~ age + poly(duration, 14) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = Data)
glm_indikator_poly16 <- glm(O ~ age + poly(duration, 16) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = Data)

glm_indikator_poly18 <- glm(O ~ age + poly(duration, 18) + I(duration >= 2/12), offset = log(E), 
                            family = poisson, data = Data)

AIC(glm_indikator_poly1,glm_indikator_poly2,glm_indikator_poly4,glm_indikator_poly6,glm_indikator_poly8,glm_indikator_poly10,glm_indikator_poly12,glm_indikator_poly14,glm_indikator_poly16,glm_indikator_poly18)



######################################################
####         Prædictions- og residualplot         ####
######################################################

#Indsæt den valgre model
model <- glm_indikator_poly2

Data$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- Data %>%
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
DurAgg <- Data %>%
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

residplotduration(model)
#abline(v = my_knots, col = "green", lty = 2)



######################################################
####                  2 INDIKATOR                 ####
######################################################
glm_2indikator_poly1 <- glm(O ~ age + poly(duration, 1) + I(duration >= 2/12) + I(duration >= 5.5/12), offset = log(E), 
                            family = poisson, data = Data)
glm_2indikator_poly2 <- glm(O ~ age + poly(duration, 2) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                            family = poisson, data = Data)
glm_2indikator_poly4 <- glm(O ~ age + poly(duration, 4) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                            family = poisson, data = Data)
glm_2indikator_poly6 <- glm(O ~ age + poly(duration, 6) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                            family = poisson, data = Data)
glm_2indikator_poly8 <- glm(O ~ age + poly(duration, 8) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                            family = poisson, data = Data)
glm_2indikator_poly10 <- glm(O ~ age + poly(duration, 10) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                             family = poisson, data = Data)

glm_2indikator_poly12 <- glm(O ~ age + poly(duration, 12) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                             family = poisson, data = Data)

glm_2indikator_poly14 <- glm(O ~ age + poly(duration, 14) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                             family = poisson, data = Data)
glm_2indikator_poly16 <- glm(O ~ age + poly(duration, 16) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                             family = poisson, data = Data)

glm_2indikator_poly18 <- glm(O ~ age + poly(duration, 18) + I(duration >= 2/12)+ I(duration >= 5.5/12), offset = log(E), 
                             family = poisson, data = Data)

AIC(glm_2indikator_poly1,glm_2indikator_poly2,glm_2indikator_poly4,glm_2indikator_poly6,glm_2indikator_poly8,glm_2indikator_poly10,glm_2indikator_poly12,glm_2indikator_poly14,glm_2indikator_poly16,glm_2indikator_poly18)





######################################################
####         Prædictions- og residualplot         ####
######################################################

#Indsæt den valgte model
model <- glm_2indikator_poly6

Data$predicted_O <- predict(model, type="response")

plot(predict(model, type="response"))

#Tjek efter trends
AgeAgg <- Data %>%
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
     main = "OE-rates with 4th degree polynomial (Duration) and 2th degree polynomial (Age) over Age")

# Tilføj den korrekte splines kurve for OE-raterne
lines(AgeAgg$age, AgeAgg$predicted_OE, col = "red", lwd = 2)



#Tjek efter trends
DurAgg <- Data %>%
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
     main = "OE-rates with 4th degree polynomial (Duration) and 2th degree polynomial (Age)over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)

residplotduration(model)
#abline(v = my_knots, col = "green", lty = 2)





######################################################
####     Hvad sker der efter duration = 3 år      ####
######################################################

# Definer MAX antallet af polynomiumgrader
num_plots <- 18

# Sekvenser for alder og varighed
xseq <- seq(30, 67, 0.1)  # Alder fra 30 til 67
useq <- seq(0, 37, 0.1)   # Varighed fra 0 til 37

# Funktion til at beregne prædiktioner baseret på en polynomiumgrad i
predmodel_i <- function(x, u, i) {
  # Opret 'new_data' med variablerne 'age' og 'duration_piecewise'
  new_data <- data.frame(
    age = x + u,        # Alder justeret for duration
    duration = u,       # Duration
    E = 1               # Eksponering sat til 1 for prædiktion
  )
  
  # Fit modellen med variabel polynomiumgrad for 'duration'
  model <- glm(O ~ poly(age, 2) + poly(duration, i) + I(duration >= 2/12) + I(duration >= 5.5/12), 
               offset = log(E), family = poisson, data = Data)
  
  # Lav prædiktioner ved hjælp af modellen
  pred <- predict(model, newdata = new_data, type = "response")
  
  return(as.numeric(pred))  # Returner prædiktionsværdien som et numerisk resultat
}

# Vis plots i grupper af 4
for (start in seq(1, num_plots, 4)) {
  # Sæt op til at vise 4 plots i et vindue
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  # Plot for hver polynomiumgrad i den aktuelle gruppe
  for (i in start:min(start + 3, num_plots)) {
    # Vectorize funktionen for den aktuelle polynomiumgrad
    predmodel_vec <- Vectorize(function(x, u) predmodel_i(x, u, i), vectorize.args = c("x", "u"))
    
    # Beregn prædiktioner
    predictions <- predmodel_vec(x = xseq, u = useq)
    
    # Plot resultatet
    plot(predictions, type = "l", col = "steelblue", lwd = 2, 
         xlab = "Index", ylab = "Predicted OE Rate", 
         main = paste("Degree", i))
    
    # Beregn den ønskede x-position for lodret linje
    index_position <- 3 / 0.1  # Den ønskede index-position
    
    # Tilføj lodret linje ved den beregnede position
    abline(v = index_position, col = "pink", lwd = 2, lty = 2)
    text(x = index_position, y = max(predictions), labels = "Varighed: 3 år", pos = 4, col = "hotpink", cex = 0.8)
  }
}

# Nulstil grafiske parametre til standard
par(mfrow = c(1, 1))

# Tilføj kode til at vise hvert plot enkeltvist
for (i in 1:num_plots) {
  # Vectorize funktionen for den aktuelle polynomiumgrad
  predmodel_vec <- Vectorize(function(x, u) predmodel_i(x, u, i), vectorize.args = c("x", "u"))
  
  # Beregn prædiktioner
  predictions <- predmodel_vec(x = xseq, u = useq)
  
  # Plot resultatet enkeltvist
  plot(predictions, type = "l", col = "steelblue", lwd = 2, 
       xlab = "Index", ylab = "Predicted OE Rate", 
       main = paste("Degree", i))
  
  # Beregn den ønskede x-position for lodret linje
  index_position <- 3 / 0.1  # Den ønskede index-position
  
  # Tilføj lodret linje ved den beregnede position
  abline(v = index_position, col = "pink", lwd = 2, lty = 2)
  text(x = index_position, y = max(predictions), labels = "Varighed: 3 år", pos = 4, col = "hotpink", cex = 0.8)
}





