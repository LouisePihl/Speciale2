FJJA <- read_csv("Data/FJ/FJJA.csv")
FJJA$OE<-FJJA$O/FJJA$E
FJJA_original<-FJJA
#Centrerer age
unique_ages_FJJA <- unique(FJJA$age)
midpoints_FJJA <- (unique_ages_FJJA[-1] + unique_ages_FJJA[-length(unique_ages_FJJA)]) / 2
#custom_last_point_FJJA <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FJJA <- c(midpoints_FJJA, custom_last_point_FJJA)
FJJA$age <- midpoints_FJJA[match(FJJA$age, unique_ages_FJJA)]
#centrerer duration
midpointsduration_FJJA<-c((unique(FJJA$duration)[-1]+unique(FJJA$duration)[-length(unique(FJJA$duration))])/2,unique(FJJA$duration)[length(unique(FJJA$duration))])
FJJA$duration<-midpointsduration_FJJA[match(FJJA$duration, unique(FJJA$duration))]
#fjerner sidste datapunkt for duration
FJJA<-FJJA[FJJA$duration!=unique(FJJA$duration)[length(unique(FJJA$duration))],]

ggplot(FJJA, aes(x = duration, y=OE)) +
  geom_point(aes(color = age))+
  facet_wrap(~ age)
#Model fit

residplotduration<-function(model){qplot(FJJA$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("duration") +
    ylab("fitted values")}

#Simpel additiv model
modeladd<-glm(O~duration+offset(log(E)),family = poisson(link="log"),data=FJJA)
summary(modeladd)
AIC(modeladd)
residplotduration(modeladd)

modelpoly2<-glm(O~poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=FJJA)
summary(modelpoly2)
AIC(modelpoly2)
residplotduration(modelpoly2)

modelint<-glm(O~offset(log(E)),family = poisson(link="log"),data=FJJA)
summary(modelint)
AIC(modelint)
residplotduration(modelint)

modelns2<-glm(O~ns(duration,2)+offset(log(E)),family = poisson(link="log"),data=FJJA)
summary(modelns2)
AIC(modelns2)
residplotduration(modelns2)