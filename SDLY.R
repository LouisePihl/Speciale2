SDLY <- read_csv("Data/SD/SDLY.csv")
SDLY$OE<-SDLY$O/SDLY$E
midpointsage<-c((unique(SDLY$age)[-1]+unique(SDLY$age)[-length(unique(SDLY$age))])/2,(unique(SDLY$age)[length(unique(SDLY$age))]+67)/2)
SDLY$age<-midpointsage[match(SDLY$age, unique(SDLY$age))]
midpointsduration<-c((unique(SDLY$duration)[-1]+unique(SDLY$duration)[-length(unique(SDLY$duration))])/2,unique(SDLY$duration)[length(unique(SDLY$duration))])
SDLY$duration<-midpointsduration[match(SDLY$duration, unique(SDLY$duration))]

#PLot af OE rater
plot(SDLY$OE)

ggplot(SDLY, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(SDLY, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- SDLY %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- SDLY %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#Model fit
residplotage<-function(model){qplot(SDLY$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(SDLY$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#fjerner sidste datapunkt for duration
SDLY<-SDLY[SDLY$duration!=unique(SDLY$duration)[length(unique(SDLY$duration))],]

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=SDLY)
summary(modeladd)

residplotage(modeladd)
residplotduration(modeladd)
AIC(modeladd)

#Polynomier
modelpoly2<-glm(O~age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDLY)
residplotduration(modelpoly2)
AIC(modelpoly2)

#modelpoly3<-glm(O~age+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDLY)
#summary(modelpoly3)
#residplotduration(modelpoly3)
#AIC(modelpoly3)

modelpoly22<-glm(O~poly(age,2)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDLY)
residplotage(modelpoly22)
AIC(modelpoly22)

modelpoly22_ind<-glm(O~poly(age,2)+I(age>=60)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDLY)
residplotage(modelpoly22_ind)
AIC(modelpoly22_ind)
summary(modelpoly22_ind)

modelpoly21_ind<-glm(O~poly(age,1)+I(age>=60)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDLY)
residplotage(modelpoly21_ind)
AIC(modelpoly21_ind)

#Indsæt den valgre model
model <- modelpoly22_ind

SDLY$predicted_O <- predict(model, type="response")


#Tjek efter trends
AgeAgg <- SDLY %>%
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
DurAgg <- SDLY %>%
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

