library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)
SDRF <- read_csv("SD/SDRF.csv")
SDRF$OE<-SDRF$O/SDRF$E
midpointsage<-c((unique(SDRF$age)[-1]+unique(SDRF$age)[-length(unique(SDRF$age))])/2,unique(SDRF$age)[length(unique(SDRF$age))])
SDRF$age_centered<-midpointsage[match(SDRF$age, unique(SDRF$age))]
midpointsduration<-c((unique(SDRF$duration)[-1]+unique(SDRF$duration)[-length(unique(SDRF$duration))])/2,unique(SDRF$duration)[length(unique(SDRF$duration))])
SDRF$duration_centered<-midpointsage[match(SDRF$duration, unique(SDRF$duration))]

#PLot af OE rater
plot(SDRF$OE)

ggplot(SDRF, aes(x = age, y=OE)) +
  geom_point(aes(color = duration))+
  facet_wrap(~ duration)

ggplot(SDRF, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)

AgeAgg <- SDRF %>% 
  group_by(age) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(AgeAgg$age,AgeAgg$OE)

DurAgg <- SDRF %>% 
  group_by(duration) %>% 
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

plot(DurAgg$duration,DurAgg$OE)

#Model fit
residplotage<-function(model){qplot(SDRF$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

residplotduration<-function(model){qplot(SDRF$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modeladd)
drop1(modeladd)

residplotage(modeladd)
residplotduration(modeladd)
AIC(modeladd)
BIC(modeladd)

#Tilføjer potensled
modelp2<-glm(O~age+duration+I(duration^2)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelp2)
AIC(modelp2)
BIC(modelp2)
deviance(modelp2)
modelp3<-glm(O~age+duration+I(duration^2)+I(duration^3)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelp3)
AIC(modelp3)
BIC(modelp3)
deviance(modelp3)

#tilføjer ortogonale polynomier på duration
modelpoly2<-glm(O~age+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly2)
residplotduration(modelpoly2)
AIC(modelpoly2)
BIC(modelpoly2)
modelpoly3<-glm(O~age+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly3)
residplotduration(modelpoly3)
AIC(modelpoly3)
BIC(modelpoly3)
modelpoly4<-glm(O~age+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly4)
residplotduration(modelpoly4)
AIC(modelpoly4)
BIC(modelpoly4)
deviance(modelpoly4)
modelpoly5<-glm(O~age+poly(duration,5)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly5)
residplotduration(modelpoly5)
AIC(modelpoly5)
BIC(modelpoly5)
deviance(modelpoly5)

#Tilføljer poly på age også
modelpoly42<-glm(O~poly(age,2)+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly42)
residplotduration(modelpoly42)
AIC(modelpoly42)
BIC(modelpoly42)
deviance(modelpoly42)

modelpoly43<-glm(O~poly(age,3)+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly43)
residplotduration(modelpoly43)
AIC(modelpoly43)
BIC(modelpoly43)
deviance(modelpoly43)

modelpoly44<-glm(O~poly(age,4)+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly44)
residplotduration(modelpoly44)
AIC(modelpoly44)
BIC(modelpoly44)
deviance(modelpoly44)

modelpoly45<-glm(O~poly(age,5)+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly45)
residplotduration(modelpoly45)
AIC(modelpoly45)
BIC(modelpoly45)
deviance(modelpoly45)

modelpoly46<-glm(O~poly(age,6)+poly(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelpoly46)
residplotduration(modelpoly46)
AIC(modelpoly46)
BIC(modelpoly46)
deviance(modelpoly46)

#Tilføjer splines på duration
modelns2<-glm(O~age+ns(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns2)
residplotduration(modelns2)
AIC(modelns2)
BIC(modelns2)
deviance(modelns2)
modelns3<-glm(O~age+ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns3)
residplotduration(modelns3)
AIC(modelns3)
BIC(modelns3)
deviance(modelns3)
#vælger knuderne ved 2 måneder og 5.5 måneder (samme antal som ovenfor)
modelns3v2<-glm(O~age+ns(duration, knots = c(2/12,5.5/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns3v2)
residplotduration(modelns3v2)
AIC(modelns3v2)
BIC(modelns3v2)
deviance(modelns3v2)

modelns4<-glm(O~age+ns(duration,4)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns4)
residplotduration(modelns4)
AIC(modelns4)
BIC(modelns4)
deviance(modelns4)
modelns5<-glm(O~age+ns(duration,5)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns5)
residplotduration(modelns5)
AIC(modelns5)
BIC(modelns5)
deviance(modelns5)
modelns8<-glm(O~age+ns(duration,8)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns8)
residplotduration(modelns8)
AIC(modelns8)
BIC(modelns8)
deviance(modelns8)
modelns9<-glm(O~age+ns(duration,9)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns9)
residplotduration(modelns9)
AIC(modelns9)
BIC(modelns9)
deviance(modelns9)

#Tilføjer splines på age også
modelns82<-glm(O~ns(age,2)+ns(duration,8)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns82)
residplotduration(modelns82)
AIC(modelns82)
BIC(modelns82)
deviance(modelns82)

modelns84<-glm(O~ns(age,4)+ns(duration,8)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns84)
residplotduration(modelns84)
AIC(modelns84)
BIC(modelns84)
deviance(modelns84)

modelns86<-glm(O~ns(age,6)+ns(duration,8)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns86)
residplotduration(modelns86)
AIC(modelns86)
BIC(modelns86)
deviance(modelns86)

modelns87<-glm(O~ns(age,7)+ns(duration,8)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelns87)
residplotduration(modelns87)
AIC(modelns87)
BIC(modelns87)
deviance(modelns87)

#Andre ikke lineære transformationer
#sinus
modelsin<-glm(O~age+sin(duration)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelsin)
residplotduration(modelsin)
AIC(modelsin)
BIC(modelsin)
deviance(modelsin)

modelsin2<-glm(O~sin(age)+sin(duration)+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelsin2)
AIC(modelsin2)
BIC(modelsin2)
deviance(modelsin2)

#log
modelsin<-glm(O~log(age)+duration+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modellog)
AIC(modellog)
BIC(modellog)
deviance(modellog)
#Kan ikke tage log på duration da vi har 0 værdier

#indikator funktion
modeladdind<-glm(O~age+duration+I((duration > 2/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modeladdind)
AIC(modeladdind)
BIC(modeladdind)
deviance(modeladdind)

#indikator ved 8. uge da man skal have haft samtale med kommunen her
modelind<-glm(O~age+I((duration > 2/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelind)
AIC(modelind)
BIC(modelind)
deviance(modelind)

#indikator ved 5.5 måned, da det som udgangspunkt er maks tid på SD
modelind2<-glm(O~age+I((duration > 2/12))+I((duration > 5.5/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelind2)
AIC(modelind2)
BIC(modelind2)
deviance(modelind2)

modelind2ns6<-glm(O~ns(age,6)+I((duration > 2/12))+I((duration > 5.5/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelind2ns6)
AIC(modelind2ns6)
BIC(modelind2ns6)
deviance(modelind2ns6)

modelind2ns68<-glm(O~ns(age,6)+ns(duration,8)+I((duration > 2/12))+I((duration > 5.5/12))+offset(log(E)),family = poisson(link="log"),data=SDRF)
summary(modelind2ns68)
AIC(modelind2ns68)
BIC(modelind2ns68)
deviance(modelind2ns68)