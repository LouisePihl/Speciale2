SDFJ <- read_csv("Data/SD/SDFJ.csv")
SDFJ$OE<-SDFJ$O/SDFJ$E
midpointsage<-c((unique(SDFJ$age)[-1]+unique(SDFJ$age)[-length(unique(SDFJ$age))])/2,(unique(SDFJ$age)[length(unique(SDFJ$age))]+67)/2)
SDFJ$age<-midpointsage[match(SDFJ$age, unique(SDFJ$age))]
midpointsduration<-c((unique(SDFJ$duration)[-1]+unique(SDFJ$duration)[-length(unique(SDFJ$duration))])/2,unique(SDFJ$duration)[length(unique(SDFJ$duration))])
SDFJ$duration<-midpointsduration[match(SDFJ$duration, unique(SDFJ$duration))]

#PLot af OE rater
plot(SDFJ$OE)

ggplot(SDFJ, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(SDFJ, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- SDFJ %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- SDFJ %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#fjerner sidste datapunkt for duration
SDFJ<-SDFJ[SDFJ$duration!=unique(SDFJ$duration)[length(unique(SDFJ$duration))],]

#Model fit
residplotage<-function(model){qplot(SDFJ$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(SDFJ$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

#Polynomier
modelpoly2<-glm(O~age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly2)
residplotduration(modelpoly2)

modelpoly3<-glm(O~age+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly3)
residplotduration(modelpoly3)

modelpoly2_ind<-glm(O~I(age>=60)+age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly2_ind)
residplotage(modelpoly2_ind)

modelpoly22<-glm(O~poly(age,2)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly22)
residplotage(modelpoly22)

modelpoly22_ind<-glm(O~I(age>=60)+poly(age,2)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly22_ind)
residplotage(modelpoly22_ind)

modelpoly23<-glm(O~poly(age,3)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modelpoly23)
residplotage(modelpoly23)

#Prædiktionsplot

SDFJ$predicted_O <- predict(modelpoly22_ind, type="response")

#Tjek efter trends
AgeAgg <- SDFJ %>%
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
DurAgg <- SDFJ %>%
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
     main = "OE-rates with 16nd degree polynomial over Duration")

# Tilføj den korrekte splines kurve for OE-raterne
lines(DurAgg$duration, DurAgg$predicted_OE, col = "red", lwd = 2)