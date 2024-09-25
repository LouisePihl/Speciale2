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
#Prædiktionsplot
predplot_age<-function(model){SDFJ$predicted_O <- predict(model, type="response")
SDFJ$predicted_OE <- SDFJ$predicted_O/SDFJ$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(SDFJ$age, SDFJ$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Age",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(SDFJ$age, Data$predicted_OE, col = "red", lwd = 2)}

predplot_dur<-function(model){SDFJ$predicted_O <- predict(model, type="response")
SDFJ$predicted_OE <- SDFJ$predicted_O/SDFJ$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(SDFJ$age, SDFJ$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(SDFJ$duration, SDFJ$predicted_OE, col = "red", lwd = 2)}

#Simpel additiv model
modeladd<-glm(O~age+duration+offset(log(E)),family = poisson(link="log"),data=SDFJ)
AIC(modeladd)
residplotage(modeladd)
residplotduration(modeladd)
