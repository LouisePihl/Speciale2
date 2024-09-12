Data <- read_csv("LY/LYFP.csv")
Data$OE<-Data$O/Data$E
Data<-Data[-1,]
Data<-Data[-c(1,29,30,31,32),]
Data$age[1]<-(40+17)/2

plot(Data$age,Data$OE)
plot(Data$age,Data$E)

#Model fit
residplotage<-function(model){qplot(Data$age, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}
#Prædiktionsplot
predplot<-function(model){Data$predicted_O <- predict(model, type="response")
Data$predicted_OE <- Data$predicted_O/Data$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(Data$age, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Age",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(Data$age, Data$predicted_OE, col = "red", lwd = 2)}


#Simpel additiv model
modeladd<-glm(O~age+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modeladd)
residplotage(modeladd)

modelpoly2<-glm(O~poly(age,2)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly2)
residplotage(modelpoly2)
predplot(modelpoly2)

modelpoly3<-glm(O~I(age<57) + poly(I(age*(age >= 57)), 3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly3)
AIC(modelpoly3)
residplotage(modelpoly3)
predplot(modelpoly3)

modelpoly4<-glm(O~poly(age,4)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly4)
residplotage(modelpoly4)
predplot(modelpoly4)

modelpoly5<-glm(O~poly(age,5)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly5)
residplotage(modelpoly5)
predplot(modelpoly5)

modelpoly6<-glm(O~poly(age,6)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly6)
residplotage(modelpoly6)
predplot(modelpoly6)

modelpoly7<-glm(O~poly(age,7)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly7)
residplotage(modelpoly7)

modelpoly8<-glm(O~poly(age,8)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelpoly8)
residplotage(modelpoly8)

modelns2<-glm(O~ns(age,2)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelns2)
residplotage(modelns2)
predplot(modelns2)

modelns4<-glm(O~ns(age,4)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelns4)
residplotage(modelns4)
predplot(modelns4)

modelns6<-glm(O~ns(age,6)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelns6)
residplotage(modelns6)
predplot(modelns6)

modelns8<-glm(O~ns(age,8)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelns8)
residplotage(modelns8)
predplot(modelns8)

modelns10<-glm(O~ns(age,10)+offset(log(E)),family = poisson(link="log"),data=Data)
AIC(modelns10)
residplotage(modelns10)
predplot(modelns10)

#Indikator
modelpoly3<-glm(O~I(age<56)*age + poly(I(age*(age >= 56)), 3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly3)
AIC(modelpoly3) 
predplot(modelpoly3)

modelpoly3<-glm(O~I(age<57)+ poly(I(age*(age >= 57)), 3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly3)
AIC(modelpoly3)
predplot(modelpoly3)

modelpoly3<-glm(O~age+ poly(I(age*(age >= 57)), 3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly3)
AIC(modelpoly3)
predplot(modelpoly3)
