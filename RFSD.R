Data <- read_csv("Data/RF/RFSD.csv")
Data$OE <- Data$O/Data$E

#OE-raten for varighed 3 (endepunktet):
Data_filtered <- Data[Data$duration == 3, ]
Data_filtered$OE <- Data_filtered$O/Data_filtered$E
Data_OE_endpoint <- sum(Data_filtered$O)/sum(Data_filtered$E)

#centrerer duration
midpointsduration_Data <- c((unique(Data$duration)[-1] + unique(Data$duration)[-length(unique(Data$duration))]) / 2, unique(Data$duration)[length(unique(Data$duration))])
Data$duration <- midpointsduration_Data[match(Data$duration, unique(Data$duration))]

#fjerner sidste datapunkt for duration
Data <- Data[Data$duration != unique(Data$duration)[length(unique(Data$duration))], ]

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

#Simpel model
modelsimp<-glm(O ~ offset(log(E)), family = poisson, data=Data)
summary(modelsimp)
residplotduration(modelsimp)
predplot(modelsimp)
AIC(modeladd)

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


#Splines
modelns2<-glm(O~ns(duration,2)+offset(log(E)),family = poisson(link="log"),data=RFSD)
summary(modelns2)

residplotduration(modelns2)
predplot(modelns2)
AIC(modelns2)

modelns3<-glm(O~ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=RFSD)
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


