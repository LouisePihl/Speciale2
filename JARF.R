JARF <- read_csv("Data/JA/JARF.csv")
JARF$OE<-JARF$O/JARF$E
midpointsage<-c((unique(JARF$age)[-1]+unique(JARF$age)[-length(unique(JARF$age))])/2,(unique(JARF$age)[length(unique(JARF$age))]+67)/2)
JARF$age<-midpointsage[match(JARF$age, unique(JARF$age))]
midpointsduration<-c((unique(JARF$duration)[-1]+unique(JARF$duration)[-length(unique(JARF$duration))])/2,unique(JARF$duration)[length(unique(JARF$duration))])
JARF$duration<-midpointsduration[match(JARF$duration, unique(JARF$duration))]

ggplot(JARF, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(JARF, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- JARF %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- JARF %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#fjerner sidste datapunkt for duration
JARF<-JARF[JARF$duration!=unique(JARF$duration)[length(unique(JARF$duration))],]

#Model fit
residplotage<-function(model){qplot(JARF$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(JARF$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

modelpoly2<-glm(O~poly(age,2)+duration+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly2)
residplotage(modelpoly2)

modelpoly3<-glm(O~poly(age,3)+duration+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly3)
residplotage(modelpoly3)

modelpoly4<-glm(O~poly(age,4)+duration+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly4)
residplotage(modelpoly4)

modelpoly42<-glm(O~poly(age,4)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly42)
residplotduration(modelpoly42)

modelpoly43<-glm(O~poly(age,4)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly43)
residplotduration(modelpoly43)

modelpoly4ns3<-glm(O~poly(age,4)+ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=JARF)
AIC(modelpoly4ns3)
residplotduration(modelpoly4ns3)

#Prædiktionsplot

JARF$predicted_O <- predict(modelpoly3, type="response")

#Tjek efter trends
AgeAgg <- JARF %>%
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
DurAgg <- JARF %>%
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
