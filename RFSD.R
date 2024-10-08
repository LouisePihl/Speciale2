Data <- read_csv("Data/RF/RFSD.csv")
view(Data)
Data$OE<-Data$O/Data$E

plot(Data$duration,Data$OE)
plot(Data$duration,Data$E)

#Model fit funktioner
residplotduration<-function(model){qplot(Data$duration, .stdresid,data = model)+
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    xlab("age") +
    ylab("fitted values")}

#Prædiktionsplot
predplot<-function(model){Data$predicted_O <- predict(model, type="response")
Data$predicted_OE <- Data$predicted_O/Data$E
# Beregn OE-rater med de korrekte E-værdier

# Plot OE-rates over age
plot(Data$duration, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(Data$duration, Data$predicted_OE, col = "red", lwd = 2)}

#Additiv model
modeladd<-glm(O~duration+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modeladd)

residplotduration(modeladd)
predplot(modeladd)
AIC(modeladd)

#Polynomier
modelpoly2<-glm(O~poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly2)

residplotduration(modelpoly2)
predplot(modelpoly2)
AIC(modelpoly2)

modelpoly3<-glm(O~poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelpoly3)

residplotduration(modelpoly3)
predplot(modelpoly3)
AIC(modelpoly3)

#Splines
modelns2<-glm(O~ns(duration,2)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelns2)

residplotduration(modelns2)
predplot(modelns2)
AIC(modelns2)

modelns3<-glm(O~ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=Data)
summary(modelns3)

residplotduration(modelns3)
predplot(modelns3)
AIC(modelns3)

dur_seq<-seq(0,3,0.1)
new_data <- data.frame(
  duration = dur_seq,
  E=1
)
pred<-predict(modelpoly2, newdata = new_data, type = "response")

plot(Data$duration, Data$OE, 
     col = "blue", 
     pch = 1, 
     xlab = "Duration",
     ylab = "OE Rate",
     main = "OE-rates with additive model over Duration")
lines(dur_seq, pred, col = "red", lwd = 2)

