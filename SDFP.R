SDFP <- read_csv("Data/SD/SDFP.csv")
SDFP$OE<-SDFP$O/SDFP$E
midpointsage<-c((unique(SDFP$age)[-1]+unique(SDFP$age)[-length(unique(SDFP$age))])/2,(unique(SDFP$age)[length(unique(SDFP$age))]+67)/2)
SDFP$age<-midpointsage[match(SDFP$age, unique(SDFP$age))]
midpointsduration<-c((unique(SDFP$duration)[-1]+unique(SDFP$duration)[-length(unique(SDFP$duration))])/2,unique(SDFP$duration)[length(unique(SDFP$duration))])
SDFP$duration<-midpointsduration[match(SDFP$duration, unique(SDFP$duration))]

ggplot(SDFP, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(SDFP, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- SDFP %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- SDFP %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#fjerner sidste datapunkt for duration
SDFP<-SDFP[SDFP$duration!=unique(SDFP$duration)[length(unique(SDFP$duration))],]

#fjerner første datapunkt for age
SDFP<-SDFP[SDFP$age!=unique(SDFP$age)[1],]

#Model fit
residplotage<-function(model){qplot(SDFP$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(SDFP$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Modelfit
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

modelpoly2<-glm(O~age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelpoly2)
residplotduration(modelpoly2)

modelpoly3<-glm(O~age+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelpoly3)
residplotduration(modelpoly3)

modelpoly32<-glm(O~poly(age,2)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelpoly32)
residplotduration(modelpoly32)

modelpoly33<-glm(O~poly(age,3)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelpoly33)
residplotduration(modelpoly33)

modelpoly3ns3<-glm(O~ns(age,3)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelpoly3ns3)
residplotduration(modelpoly3ns3)
summary(modelpoly3ns3)

modelns3ns3<-glm(O~ns(age,3)+ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)
AIC(modelns3ns3)
residplotduration(modelns3ns3)
summary(modelns3ns3)

#Prædiktionsplot

SDFP$predicted_O <- predict(modelns3ns3, type="response")

#Tjek efter trends
AgeAgg <- SDFP %>%
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
DurAgg <- SDFP %>%
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
