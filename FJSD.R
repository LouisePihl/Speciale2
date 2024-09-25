######################################################
####                  DATA LOADING                ####
######################################################

# Pakker: 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

# Indlæs data
Data <- read_csv("Data/FJ/FJSD.csv")
Data$OE <- Data$O / Data$E

View(Data)

#Dette datasæt har ingen inddeling af age, så vi har kun covariaten duration.

######################################################
####            VISUALISERING AF DATA             ####
######################################################

# Plot data
plot(y = Data$O, x = Data$duration)
plot(y = Data$OE, x = Data$duration)


######################################################
####          CENTRERING AF COVARIATER            ####
######################################################

#centrerer duration
midpointsduration <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration <- midpointsduration[match(Data$duration, unique(Data$duration))]

#fjerner sidste datapunkt for duration
Data <- Data[Data$duration != unique(Data$duration)[length(unique(Data$duration))], ]

# Vis centreret data
View(Data)


######################################################
####               LINEAR GLM MODEL               ####
######################################################

# Lineær GLM model
glm_linear <- glm(O ~ duration, offset = log(E), 
                  family = poisson, data = Data)

# Forudsigelser med lineær model
Data$predicted_O <- predict(glm_linear, type = "response")
Data$predicted_OE <- Data$predicted_O / Data$E

# Plot OE-rates over duration med lineær model
plot(Data$duration, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(Data$duration, Data$predicted_OE, col = "red", lwd = 2)


######################################################
####                  RESIDUAL PLOT               ####
######################################################

# Funktion til residual plot
residplotduration <- function(model) {
  qplot(Data$duration, .stdresid, data = model) +
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("Duration") +
    ylab("Fitted Values")
}

# Plot residuals for linear model
residplotduration(glm_linear)



######################################################
####                  POLYNIMIER                  ####
######################################################

# Polynomial modeller for duration
glm_poly2 <- glm(O ~ poly(duration, 2), offset = log(E), 
                 family = poisson, data = Data)

glm_poly3 <- glm(O ~ poly(duration, 3), offset = log(E), 
                 family = poisson, data = Data)

glm_poly4 <- glm(O ~ poly(duration, 4), offset = log(E), 
                 family = poisson, data = Data)

glm_poly6 <- glm(O ~ poly(duration, 6), offset = log(E), 
                 family = poisson, data = Data)

glm_poly8 <- glm(O ~ poly(duration, 8), offset = log(E), 
                 family = poisson, data = Data)

glm_poly10 <- glm(O ~ poly(duration, 10), offset = log(E), 
                  family = poisson, data = Data)

# Sammenlign AIC for polynomial modeller
AIC(glm_linear, glm_poly2, glm_poly3, glm_poly4, glm_poly6, glm_poly8, glm_poly10)

# Forudsigelser med 2. grad polynomium model
Data$predicted_O <- predict(glm_poly2, type = "response")
Data$predicted_OE <- Data$predicted_O / Data$E

# Plot OE-rates over duration med 2. grad polynomium
plot(Data$duration, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with 2nd degree polynomial over Duration")
lines(Data$duration, Data$predicted_OE, col = "red", lwd = 2)

# Plot residuals for polynomial model
residplotduration(glm_poly2)



######################################################
####                   SPLINES                    ####
######################################################

# Splines modeller for duration
glm_splines2 <- glm(O ~ ns(duration, df = 3), offset = log(E), 
                    family = poisson, data = Data)

glm_splines3 <- glm(O ~ ns(duration, df = 4), offset = log(E), 
                    family = poisson, data = Data)

glm_splines4 <- glm(O ~ ns(duration, df = 5), offset = log(E), 
                    family = poisson, data = Data)

glm_splines5 <- glm(O ~ ns(duration, df = 6), offset = log(E), 
                    family = poisson, data = Data)

glm_splines6 <- glm(O ~ ns(duration, df = 7), offset = log(E), 
                    family = poisson, data = Data)

# Sammenlign AIC for splines modeller
AIC(glm_poly2, glm_splines2, glm_splines3, glm_splines4, glm_splines5, glm_splines6)



######################################################
####                    HEATMAP                   ####
######################################################

# Beregn de forudsigede rater fra GLM-modellen
Data$observed_rate <- Data$O / Data$E
Data$predicted_rate <- predict(glm_poly2, newdata = Data, type = "response")
Data$predicted_rate_oe <- Data$predicted_rate / Data$E

# Heatmap for de observerede rater
heatmap_observed <- ggplot(Data, aes(y = duration, x = 1, fill = observed_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Observerede OE-Rater",
       y = "Duration",
       x = "1",
       fill = "Observed OE-Rate") +
  theme_minimal()

# Heatmap for de forudsigede rater
heatmap_predicted <- ggplot(Data, aes(y = duration, x = 1, fill = predicted_rate_oe)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap af Predicted OE-Rates",
       y = "Duration",
       x = "1",
       fill = "Predicted OE-Rate") +
  theme_minimal()

# Print begge heatmaps
heatmap_observed
heatmap_predicted

# Kombiner heatmaps i et enkelt plot
library(gridExtra)
grid.arrange(heatmap_observed, heatmap_predicted, ncol = 2)


# Beregn forskellen mellem observerede og forudsigede rater
Data$difference_rate <- Data$observed_rate - Data$predicted_rate_oe

# Heatmap for forskelle mellem observerede og forudsigede rater
ggplot(Data, aes(x = 1, y = duration, fill = difference_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap af Forskel mellem Observerede og Forudsigede Rater",
       x = "1",
       y = "Duration",
       ylim = c(0, 5.5 / 12),
       fill = "Forskel Rate") +
  theme_minimal()



######################################################
####              ENDELIGE MODEL                  ####
######################################################

glm_FJSD <- glm(O ~ poly(duration,2), offset = log(E), 
                  family = poisson, data = Data)





