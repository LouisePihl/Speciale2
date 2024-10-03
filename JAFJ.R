JAFJ <- read_csv("Data/JA/JAFJ.csv")
JAFJ$OE<-JAFJ$O/JAFJ$E
midpointsage<-c((unique(JAFJ$age)[-1]+unique(JAFJ$age)[-length(unique(JAFJ$age))])/2,(unique(JAFJ$age)[length(unique(JAFJ$age))]+67)/2)
JAFJ$age<-midpointsage[match(JAFJ$age, unique(JAFJ$age))]
midpointsduration<-c((unique(JAFJ$duration)[-1]+unique(JAFJ$duration)[-length(unique(JAFJ$duration))])/2,unique(JAFJ$duration)[length(unique(JAFJ$duration))])
JAFJ$duration<-midpointsduration[match(JAFJ$duration, unique(JAFJ$duration))]

ggplot(JAFJ, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(JAFJ, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- JAFJ %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- JAFJ %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#fjerner sidste datapunkt for duration
JAFJ<-JAFJ[JAFJ$duration!=unique(JAFJ$duration)[length(unique(JAFJ$duration))],]

#Model fit
residplotage<-function(model){qplot(JAFJ$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(JAFJ$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

modelpoly2<-glm(O~poly(age,2)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly2)
residplotage(modelpoly2)

modelns2<-glm(O~ns(age,2)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelns2)
residplotage(modelns2)

modelns2_ind<-glm(O~ns(age,2)+I(age>=60)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelns2_ind)
residplotage(modelns2_ind)

modelpoly2_ind<-glm(O~poly(age,2)+I(age>=60)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly2_ind)
residplotage(modelpoly2_ind)

modelpoly3<-glm(O~poly(age,3)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly3)
residplotage(modelpoly3)

modelpoly3_ind<-glm(O~poly(age,3)+I(age>=60)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly3_ind)
residplotage(modelpoly3_ind)

modelpoly4<-glm(O~poly(age,4)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly4)
residplotage(modelpoly4)

modelpoly4_ind<-glm(O~poly(age,4)+I(age>=60)+duration+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly4_ind)
residplotage(modelpoly4_ind)

modelpoly2_ind_2<-glm(O~poly(age,2)+I(age>=60)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly2_ind_2)
residplotduration(modelpoly2_ind_2)

#endelig
modelpoly2_ind_3<-glm(O~poly(age,2)+I(age>=60)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=JAFJ)
AIC(modelpoly2_ind_3)
residplotduration(modelpoly2_ind_3)


#Prædiktionsplot

JAFJ$predicted_O <- predict(modelpoly2_ind_2, type="response")

#Tjek efter trends
AgeAgg <- JAFJ %>%
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
DurAgg <- JAFJ %>%
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

