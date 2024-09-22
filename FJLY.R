FJLY <- read_csv("Data/FJ/FJLY.csv")
FJLY$OE<-FJLY$O/FJLY$E
FJLY_original<-FJLY
#Centrerer age
unique_ages_FJLY <- unique(FJLY$age)
midpoints_FJLY <- (unique_ages_FJLY[-1] + unique_ages_FJLY[-length(unique_ages_FJLY)]) / 2
custom_last_point_FJLY <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FJLY <- c(midpoints_FJLY, custom_last_point_FJLY)
FJLY$age <- midpoints_FJLY[match(FJLY$age, unique_ages_FJLY)]
#centrerer duration
midpointsduration_FJLY<-c((unique(FJLY$duration)[-1]+unique(FJLY$duration)[-length(unique(FJLY$duration))])/2,unique(FJLY$duration)[length(unique(FJLY$duration))])
FJLY$duration<-midpointsduration_FJLY[match(FJLY$duration, unique(FJLY$duration))]
#fjerner sidste datapunkt for duration
FJLY<-FJLY[FJLY$duration!=unique(FJLY$duration)[length(unique(FJLY$duration))],]

plot(FJLY$OE)

ggplot(FJLY, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(FJLY, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- FJLY %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- FJLY_original %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#Model fit
residplotage<-function(model){qplot(FJLY$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(FJLY$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=FJLY)
summary(modeladd)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)

#polynomier på duration
modelpoly2<-glm(O~age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly2)
residplotduration(modelpoly2)

modelpoly4<-glm(O~age+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly4)
residplotduration(modelpoly4)

modelpoly5<-glm(O~age+poly(duration,5)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly5)
residplotduration(modelpoly5)

modelpoly6<-glm(O~age+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly6)
residplotduration(modelpoly6)

modelpoly7<-glm(O~age+poly(duration,7)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly7)
residplotduration(modelpoly7)

#Polynomier på age
modelpoly62<-glm(O~poly(age,2)+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly62)
residplotage(modelpoly62)

modelpoly64<-glm(O~poly(age,4)+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly64)
residplotage(modelpoly64)

modelpoly65<-glm(O~poly(age,5)+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly65)
residplotage(modelpoly65)

modelpoly66<-glm(O~poly(age,6)+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly66)
residplotage(modelpoly66)

#Splines
modelns6<-glm(O~age+ns(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelns6)
residplotduration(modelns6)

modelpoly6ns6<-glm(O~ns(age,6)+poly(duration,6)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly6ns6)
residplotage(modelpoly66)

#Endelig ud fra residualplot og AIC, inden kig på middeltider
modelpoly54<-glm(O~poly(age,4)+poly(duration,5)+offset(log(E)),family = poisson(link="log"),data=FJLY)
AIC(modelpoly54)
residplotage(modelpoly54)

