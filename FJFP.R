FJFP <- read_csv("Data/FJ/FJFP.csv")
FJFP$OE<-FJFP$O/FJFP$E
FJFP_original<-FJFP
#Centrerer age
unique_ages_FJFP <- unique(FJFP$age)
midpoints_FJFP <- (unique_ages_FJFP[-1] + unique_ages_FJFP[-length(unique_ages_FJFP)]) / 2
custom_last_point_FJFP <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FJFP <- c(midpoints_FJFP, custom_last_point_FJFP)
FJFP$age <- midpoints_FJFP[match(FJFP$age, unique_ages_FJFP)]
#centrerer duration
midpointsduration_FJFP<-c((unique(FJFP$duration)[-1]+unique(FJFP$duration)[-length(unique(FJFP$duration))])/2,unique(FJFP$duration)[length(unique(FJFP$duration))])
FJFP$duration<-midpointsduration_FJFP[match(FJFP$duration, unique(FJFP$duration))]
#fjerner sidste datapunkt for duration
FJFP<-FJFP[FJFP$duration!=unique(FJFP$duration)[length(unique(FJFP$duration))],]

#Fjerner første datapunkt for age 
FJFP<-FJFP[FJFP$age!=unique(FJFP$age)[1],]

plot(FJFP$OE)

ggplot(FJFP, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(FJFP, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- FJFP %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- FJFP_original %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#Model fit
residplotage<-function(model){qplot(FJFP$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(FJFP$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
summary(modeladd)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

#polynomier
modelpoly2<-glm(O~poly(age,2)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
summary(modelpoly2)
AIC(modelpoly2)
residplotage(modelpoly2)

modelpoly3<-glm(O~poly(age,3)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
summary(modelpoly3)
AIC(modelpoly3)
residplotage(modelpoly3)

modelpoly4<-glm(O~poly(age,4)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
summary(modelpoly4)
AIC(modelpoly4)
residplotage(modelpoly4)

modelpoly32<-glm(O~poly(age,3)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=FJFP)
AIC(modelpoly32)

#Splines
modelns2<-glm(O~ns(age,2)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
AIC(modelns2)
residplotage(modelns2)

modelns3<-glm(O~ns(age,3)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
AIC(modelns3)
residplotage(modelns3)

modelns4<-glm(O~ns(age,4)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)
AIC(modelns4)
residplotage(modelns4)
