#Pakker: 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

#Define new data to predict intensities
t_seq<-seq(t_0,t_slut,h)
u_seq<-seq(u_0,u_slut,h)
grid <- expand.grid(age = t_seq, duration = u_seq)
new_data <- data.frame(
  age = grid$age,
  duration = grid$duration,
  E=1
)

######################################
####         SYGEDAGPENGE         ####
######################################

################ SDJA ################
SDJA <- read_csv("Data/SD/SDJA.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDJA_filtered <- SDJA[SDJA$duration == 3, ]
SDJA_filtered$OE <- SDJA_filtered$O/SDJA_filtered$E
SDJA_OE_endpoint<-sum(SDJA_filtered$O)/sum(SDJA_filtered$E)

#Centrerer age
unique_ages_SDJA <- unique(SDJA$age)
midpoints_SDJA <- (unique_ages_SDJA[-1] + unique_ages_SDJA[-length(unique_ages_SDJA)]) / 2
custom_last_point_SDJA <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDJA <- c(midpoints_SDJA, custom_last_point_SDJA)
SDJA$age <- midpoints_SDJA[match(SDJA$age, unique_ages_SDJA)]

#centrerer duration
midpointsduration_SDJA<-c((unique(SDJA$duration)[-1]+unique(SDJA$duration)[-length(unique(SDJA$duration))])/2,unique(SDJA$duration)[length(unique(SDJA$duration))])
SDJA$duration<-midpointsduration_SDJA[match(SDJA$duration, unique(SDJA$duration))]

#fjerner sidste datapunkt for duration
SDJA<-SDJA[SDJA$duration!=unique(SDJA$duration)[length(unique(SDJA$duration))],]

SDJA_final <- glm(O ~ poly(age,2) + poly(duration, 4) + I(duration >= 2/12), offset = log(E), family = poisson, data = SDJA)

#For duration større end 3 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
SDJA_OE_endpoint #0.005942464

predictions <- predict(SDJA_final, newdata = new_data, type = "response")
mu_int[,,1,2]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ SDRF ################
SDRF <- read_csv("Data/SD/SDRF.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDRF_filtered <- SDRF[SDRF$duration == 3, ]
SDRF_filtered$OE <- SDRF_filtered$O/SDRF_filtered$E
SDRF_OE_endpoint<-sum(SDRF_filtered$O)/sum(SDRF_filtered$E)

#Centrerer age
unique_ages_SDRF <- unique(SDRF$age)
midpoints_SDRF <- (unique_ages_SDRF[-1] + unique_ages_SDRF[-length(unique_ages_SDRF)]) / 2
custom_last_point_SDRF <- (67 - 58) / 2 + 58  # Brug 62.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDRF <- c(midpoints_SDRF, custom_last_point_SDRF)
SDRF$age <- midpoints_SDRF[match(SDRF$age, unique_ages_SDRF)]

#centrerer duration
midpointsduration_SDRF<-c((unique(SDRF$duration)[-1]+unique(SDRF$duration)[-length(unique(SDRF$duration))])/2,unique(SDRF$duration)[length(unique(SDRF$duration))])
SDRF$duration<-midpointsduration_SDRF[match(SDRF$duration, unique(SDRF$duration))]

#fjerner sidste datapunkt for duration
SDRF<-SDRF[SDRF$duration!=unique(SDRF$duration)[length(unique(SDRF$duration))],]

SDRF_final <- glm(O~poly(age,3)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDRF)
#For duration større end 3 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
SDRF_OE_endpoint #0.003950156

predictions <- predict(SDRF_final, newdata = new_data, type = "response")
mu_int[,,1,3]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ SDLY ################
SDLY <- read_csv("Data/SD/SDLY.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDLY_filtered <- SDLY[SDLY$duration == 3, ]
SDLY_filtered$OE <- SDLY_filtered$O/SDLY_filtered$E
SDLY_OE_endpoint<-sum(SDLY_filtered$O)/sum(SDLY_filtered$E)

#Centrerer age
unique_ages_SDLY <- unique(SDLY$age)
midpoints_SDLY <- (unique_ages_SDLY[-1] + unique_ages_SDLY[-length(unique_ages_SDLY)]) / 2
custom_last_point_SDLY <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDLY <- c(midpoints_SDLY, custom_last_point_SDLY)
SDLY$age <- midpoints_SDLY[match(SDLY$age, unique_ages_SDLY)]

#centrerer duration
midpointsduration_SDLY<-c((unique(SDLY$duration)[-1]+unique(SDLY$duration)[-length(unique(SDLY$duration))])/2,unique(SDLY$duration)[length(unique(SDLY$duration))])
SDLY$duration<-midpointsduration_SDLY[match(SDLY$duration, unique(SDLY$duration))]

#fjerner sidste datapunkt for duration
SDLY<-SDLY[SDLY$duration!=unique(SDLY$duration)[length(unique(SDLY$duration))],]

SDLY_final <- glm(O~poly(age,2)+I(age>=60)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDLY)
#For duration større end 3 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
SDLY_OE_endpoint #0.01155234

predictions <- predict(SDLY_final, newdata = new_data, type = "response")
mu_int[,,1,4]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ SDFJ ################
SDFJ <- read_csv("Data/SD/SDFJ.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDFJ_filtered <- SDFJ[SDFJ$duration == 3, ]
SDFJ_filtered$OE <- SDFJ_filtered$O/SDFJ_filtered$E
SDFJ_OE_endpoint<-sum(SDFJ_filtered$O)/sum(SDFJ_filtered$E)

midpointsage<-c((unique(SDFJ$age)[-1]+unique(SDFJ$age)[-length(unique(SDFJ$age))])/2,(unique(SDFJ$age)[length(unique(SDFJ$age))]+67)/2)
SDFJ$age<-midpointsage[match(SDFJ$age, unique(SDFJ$age))]
midpointsduration<-c((unique(SDFJ$duration)[-1]+unique(SDFJ$duration)[-length(unique(SDFJ$duration))])/2,unique(SDFJ$duration)[length(unique(SDFJ$duration))])
SDFJ$duration<-midpointsduration[match(SDFJ$duration, unique(SDFJ$duration))]

#fjerner sidste datapunkt for duration
SDFJ<-SDFJ[SDFJ$duration!=unique(SDFJ$duration)[length(unique(SDFJ$duration))],]

SDFJ_final<-glm(O~I(age>=60)+poly(age,2)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=SDFJ)

#Duration over 3 år
SDFJ_OE_endpoint #0.00258375

predictions <- predict(SDFJ_final, newdata = new_data, type = "response")
mu_int[,,1,5]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ SDFP ################
SDFP <- read_csv("Data/SD/SDFP.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDFP_filtered <- SDFP[SDFP$duration == 3, ]
SDFP_filtered$OE <- SDFP_filtered$O/SDFP_filtered$E
SDFP_OE_endpoint<-sum(SDFP_filtered$O)/sum(SDFP_filtered$E)

midpointsage<-c((unique(SDFP$age)[-1]+unique(SDFP$age)[-length(unique(SDFP$age))])/2,(unique(SDFP$age)[length(unique(SDFP$age))]+67)/2)
SDFP$age<-midpointsage[match(SDFP$age, unique(SDFP$age))]
midpointsduration<-c((unique(SDFP$duration)[-1]+unique(SDFP$duration)[-length(unique(SDFP$duration))])/2,unique(SDFP$duration)[length(unique(SDFP$duration))])
SDFP$duration<-midpointsduration[match(SDFP$duration, unique(SDFP$duration))]

#fjerner sidste datapunkt for duration
SDFP<-SDFP[SDFP$duration!=unique(SDFP$duration)[length(unique(SDFP$duration))],]

#fjerner første datapunkt for age
SDFP<-SDFP[SDFP$age!=unique(SDFP$age)[1],]

SDFP_final<-glm(O~ns(age,3)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=SDFP)

#Duration over 3 år
SDFP_OE_endpoint #0.002409844

predictions <- predict(SDFP_final, newdata = new_data, type = "response")
mu_int[,,1,6]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

######################################
####         JOBAFKLARING         ####
######################################

################ JAFJ ################
JAFJ <- read_csv("Data/JA/JAFJ.csv")
midpointsage<-c((unique(JAFJ$age)[-1]+unique(JAFJ$age)[-length(unique(JAFJ$age))])/2,(unique(JAFJ$age)[length(unique(JAFJ$age))]+67)/2)
JAFJ$age<-midpointsage[match(JAFJ$age, unique(JAFJ$age))]
midpointsduration<-c((unique(JAFJ$duration)[-1]+unique(JAFJ$duration)[-length(unique(JAFJ$duration))])/2,unique(JAFJ$duration)[length(unique(JAFJ$duration))])
JAFJ$duration<-midpointsduration[match(JAFJ$duration, unique(JAFJ$duration))]
#fjerner sidste datapunkt for duration
JAFJ<-JAFJ[JAFJ$duration!=unique(JAFJ$duration)[length(unique(JAFJ$duration))],]

#OE-raten for varrighed 3 (endepunktet) for alle aldre
JAFJ_filtered <- JAFJ[JAFJ$duration == 3, ]
JAFJ_filtered$OE <- JAFJ_filtered$O/JAFJ_filtered$E
JAFJ_OE_endpoint<-sum(JAFJ_filtered$O)/sum(JAFJ_filtered$E)

JAFJ_final<-glm(O~poly(age,2)+I(age>=60)+poly(duration,3)+offset(log(E)),family = poisson(link="log"),data=JAFJ)

predictions <- predict(SDJA_final, newdata = new_data, type = "response")
mu_int[,,2,5]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ JARF ################
JARF <- read_csv("Data/JA/JARF.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
JARF_filtered <- JARF[JARF$duration == 4, ]
JARF_filtered$OE <- JARF_filtered$O/JARF_filtered$E
JARF_OE_endpoint<-sum(JARF_filtered$O)/sum(JARF_filtered$E)

#Centrerer age
unique_ages_JARF <- unique(JARF$age)
midpoints_JARF <- (unique_ages_JARF[-1] + unique_ages_JARF[-length(unique_ages_JARF)]) / 2
custom_last_point_JARF <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JARF <- c(midpoints_JARF, custom_last_point_JARF)
JARF$age <- midpoints_JARF[match(JARF$age, unique_ages_JARF)]

#centrerer duration
midpointsduration_JARF<-c((unique(JARF$duration)[-1]+unique(JARF$duration)[-length(unique(JARF$duration))])/2,unique(JARF$duration)[length(unique(JARF$duration))])
JARF$duration<-midpointsduration_JARF[match(JARF$duration, unique(JARF$duration))]

#fjerner sidste datapunkt for duration
JARF<-JARF[JARF$duration!=unique(JARF$duration)[length(unique(JARF$duration))],]

JARF_final <- glm(O~poly(age,4)+ns(duration,3)+offset(log(E)),family = poisson(link="log"),data=JARF)
#For duration større end 4 (svarende til sidste punkt, dette benyttes ikke i modellen) sæt OE-raten konstant til: 
JARF_OE_endpoint #0.03104505

predictions <- predict(JAFJ_final, newdata = new_data, type = "response")
mu_int[,,2,3]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ JALY ################
JALY <- read_csv("Data/JA/JALY.csv")

#OE-raten for varighed 4.5 (endepunktet) for alle aldre
JALY_filtered_dur <- JALY[JALY$duration == 4.5, ]
JALY_filtered_dur$OE <- JALY_filtered_dur$O/JALY_filtered_dur$E
JALY_OE_endpoint_dur <- sum(JALY_filtered_dur$O)/sum(JALY_filtered_dur$E)

#OE-raten for alder 60 (endepunktet) for alle aldre
JALY_filtered_age <- JALY[JALY$age == 60, ]
JALY_filtered_age$OE <- JALY_filtered_age$O/JALY_filtered_age$E
JALY_OE_endpoint_age <- sum(JALY_filtered_age$O)/sum(JALY_filtered_age$E)

#Centrerer age
unique_ages_JALY <- unique(JALY$age)
midpoints_JALY <- (unique_ages_JALY[-1] + unique_ages_JALY[-length(unique_ages_JALY)]) / 2
custom_last_point_JALY <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JALY <- c(midpoints_JALY, custom_last_point_JALY)
JALY$age <- midpoints_JALY[match(JALY$age, unique_ages_JALY)]

#fjerner sidste datapunkt for age
JALY <- JALY[JALY$age != unique(JALY$age)[length(unique(JALY$age))], ]

#centrerer duration
midpointsduration_JALY <- c((unique(JALY$duration)[-1] + unique(JALY$duration)[-length(unique(JALY$duration))]) / 2, unique(JALY$duration)[length(unique(JALY$duration))])
JALY$duration <- midpointsduration_JALY[match(JALY$duration, unique(JALY$duration))]

#fjerner sidste datapunkt for duration
JALY <- JALY[JALY$duration != unique(JALY$duration)[length(unique(JALY$duration))], ]

JALY_final <- glm(O ~ poly(age,2) + poly(duration, 3):I(duration <= 2), offset = log(E), 
             family = poisson, data = JALY)

#For duration større end 4.5 og 60 år (svarende til sidste punkt, dette benyttes ikke i modellen) sæt OE-raten konstant til: 
JALY_OE_endpoint_dur #0.02935461
JALY_OE_endpoint_age #0.02555102

predictions <- predict(JALY_final, newdata = new_data, type = "response")
mu_int[,,2,4]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ JAFP ################
JAFP <- read_csv("Data/JA/JAFP.csv")

view(JAFP)

#OE-raten for varighed 3 (endepunktet) for alle aldre
JAFP_filtered <- JAFP[JAFP$duration == 3, ]
JAFP_filtered$OE <- JAFP_filtered$O/JAFP_filtered$E
JAFP_OE_endpoint <- sum(JAFP_filtered$O)/sum(JAFP_filtered$E)

#OE-raten for alder 17-40 (endepunktet) for alle aldre
JAFP_filtered_age <- JAFP[JAFP$age == 17, ]
JAFP_filtered_age$OE <- JAFP_filtered_age$O/JAFP_filtered_age$E
JAFP_OE_age_17 <- sum(JAFP_filtered_age$O)/sum(JAFP_filtered_age$E)


#Centrerer age
unique_ages_JAFP <- unique(JAFP$age)
midpoints_JAFP <- (unique_ages_JAFP[-1] + unique_ages_JAFP[-length(unique_ages_JAFP)]) / 2
custom_last_point_JAFP <- (67 - 64) / 2 + 64  # Brug 65.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JAFP <- c(midpoints_JAFP, custom_last_point_JAFP)
JAFP$age <- midpoints_JAFP[match(JAFP$age, unique_ages_JAFP)]

#Fjerner første datapunkt for age
JAFP <- JAFP[JAFP$age != unique(JAFP$age)[1], ]

#centrerer duration
midpointsduration_JAFP <- c((unique(JAFP$duration)[-1] + unique(JAFP$duration)[-length(unique(JAFP$duration))]) / 2, unique(JAFP$duration)[length(unique(JAFP$duration))])
JAFP$duration <- midpointsduration_JAFP[match(JAFP$duration, unique(JAFP$duration))]

#fjerner sidste datapunkt for duration
JAFP <- JAFP[JAFP$duration != unique(JAFP$duration)[length(unique(JAFP$duration))], ]

JAFP_final <- glm(O ~ poly(age,2) + poly(duration, 2) + I(duration >= 2)*I(age >= 60), offset = log(E), 
                  family = poisson, data = JAFP)

#For duration større end 3 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JAFP_OE_endpoint #0.007713671
JAFP_OE_age_17 #0.00207733

predictions <- predict(JAFP_final, newdata = new_data, type = "response")
mu_int[,,2,6]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ JASD ################
JASD <- read_csv("Data/JA/JASD.csv")

#OE-raten for varighed 3 (endepunktet) for alle aldre
JASD_filtered <- JASD[JASD$duration == 0.75, ]
JASD_filtered$OE <- JASD_filtered$O/JASD_filtered$E
JASD_OE_endpoint <- sum(JASD_filtered$O)/sum(JASD_filtered$E)

#Centrerer age
unique_ages_JASD <- unique(JASD$age)
midpoints_JASD <- (unique_ages_JASD[-1] + unique_ages_JASD[-length(unique_ages_JASD)]) / 2
custom_last_point_JASD <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JASD <- c(midpoints_JASD, custom_last_point_JASD)
JASD$age <- midpoints_JASD[match(JASD$age, unique_ages_JASD)]

#centrerer duration
midpointsduration_JASD <- c((unique(JASD$duration)[-1] + unique(JASD$duration)[-length(unique(JASD$duration))]) / 2, unique(JASD$duration)[length(unique(JASD$duration))])
JASD$duration <- midpointsduration_JASD[match(JASD$duration, unique(JASD$duration))]

#fjerner sidste datapunkt for duration
JASD <- JASD[JASD$duration != unique(JASD$duration)[length(unique(JASD$duration))], ]

JASD_final <- glm(O ~ poly(age, 3) + duration, offset = log(E), family = poisson, data = JASD)

#For duration større end 3 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JASD_OE_endpoint #0.002209448

predictions <- predict(JASD_final, newdata = new_data, type = "response")
mu_int[,,2,1]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

######################################
####        RESSOURCEFORLØB       ####
######################################

################ RFLY ################
RFLY <- read_csv("Data/RF/RFLY.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
RFLY_filtered <- RFLY[RFLY$duration == 3, ]
RFLY_filtered$OE <- RFLY_filtered$O/RFLY_filtered$E
RFLY_OE_endpoint<-sum(RFLY_filtered$O)/sum(RFLY_filtered$E)

#Centrerer age
unique_ages_RFLY <- unique(RFLY$age)
midpoints_RFLY <- (unique_ages_RFLY[-1] + unique_ages_RFLY[-length(unique_ages_RFLY)]) / 2
custom_last_point_RFLY <- (67 - 55) / 2 + 55  # Brug 61 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_RFLY <- c(midpoints_RFLY, custom_last_point_RFLY)
RFLY$age <- midpoints_RFLY[match(RFLY$age, unique_ages_RFLY)]

#centrerer duration
midpointsduration_RFLY<-c((unique(RFLY$duration)[-1]+unique(RFLY$duration)[-length(unique(RFLY$duration))])/2,unique(RFLY$duration)[length(unique(RFLY$duration))])
RFLY$duration<-midpointsduration_RFLY[match(RFLY$duration, unique(RFLY$duration))]

#fjerner sidste datapunkt for duration
RFLY<-RFLY[RFLY$duration!=unique(RFLY$duration)[length(unique(RFLY$duration))],]

RFLY_final <- glm(O ~ poly(duration, 2), offset = log(E), family = poisson, data = RFLY)

#For duration større end 3 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
RFLY_OE_endpoint #0.0338517

predictions <- predict(RFLY_final, newdata = new_data, type = "response")
mu_int[,,3,4]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ RFFJ ################
RFFJ <- read_csv("Data/RF/RFFJ.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
RFFJ_filtered <- RFFJ[RFFJ$duration == 3, ]
RFFJ_filtered$OE <- RFFJ_filtered$O/RFFJ_filtered$E
RFFJ_OE_endpoint<-sum(RFFJ_filtered$O)/sum(RFFJ_filtered$E)

#Centrerer age
unique_ages_RFFJ <- unique(RFFJ$age)
midpoints_RFFJ <- (unique_ages_RFFJ[-1] + unique_ages_RFFJ[-length(unique_ages_RFFJ)]) / 2
custom_last_point_RFFJ <- (67 - 55) / 2 + 55  # Brug 61 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_RFFJ <- c(midpoints_RFFJ, custom_last_point_RFFJ)
RFFJ$age <- midpoints_RFFJ[match(RFFJ$age, unique_ages_RFFJ)]

#centrerer duration
midpointsduration_RFFJ<-c((unique(RFFJ$duration)[-1]+unique(RFFJ$duration)[-length(unique(RFFJ$duration))])/2,unique(RFFJ$duration)[length(unique(RFFJ$duration))])
RFFJ$duration<-midpointsduration_RFFJ[match(RFFJ$duration, unique(RFFJ$duration))]

#fjerner sidste datapunkt for duration
RFFJ<-RFFJ[RFFJ$duration!=unique(RFFJ$duration)[length(unique(RFFJ$duration))],]

RFFJ_final <- glm(O ~ poly(age, 2) + poly(duration, 2) + I(age <= 40), offset = log(E), family = poisson, data = RFFJ)

#For duration større end 3 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
RFFJ_OE_endpoint #0.01571686

predictions <- predict(RFFJ_final, newdata = new_data, type = "response")
mu_int[,,3,5]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ RFFP ################
RFFP <- read_csv("Data/RF/RFFP.csv")

#OE-raten for varrighed 3.5 (endepunktet) for alle aldre
RFFP_filtered <- RFFP[RFFP$duration == 3.5, ]
RFFP_filtered$OE <- RFFP_filtered$O/RFFP_filtered$E
RFFP_OE_endpoint<-sum(RFFP_filtered$O)/sum(RFFP_filtered$E)

#Centrerer age
unique_ages_RFFP <- unique(RFFP$age)
midpoints_RFFP <- (unique_ages_RFFP[-1] + unique_ages_RFFP[-length(unique_ages_RFFP)]) / 2
custom_last_point_RFFP <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_RFFP <- c(midpoints_RFFP, custom_last_point_RFFP)
RFFP$age <- midpoints_RFFP[match(RFFP$age, unique_ages_RFFP)]

#centrerer duration
midpointsduration_RFFP<-c((unique(RFFP$duration)[-1]+unique(RFFP$duration)[-length(unique(RFFP$duration))])/2,unique(RFFP$duration)[length(unique(RFFP$duration))])
RFFP$duration<-midpointsduration_RFFP[match(RFFP$duration, unique(RFFP$duration))]

#fjerner sidste datapunkt for duration
RFFP<-RFFP[RFFP$duration!=unique(RFFP$duration)[length(unique(RFFP$duration))],]

RFFP_final <- glm(O ~ poly(age, 3) + poly(duration, 2) + I(duration <= 3), offset = log(E), family = poisson, data = RFFP)

#For duration større end 3.5 (svarende til sidste punkt, dette benyttes ikke i modellen)  sæt OE-raten konstant til: 
RFFP_OE_endpoint #0.007848569

predictions <- predict(RFFP_final, newdata = new_data, type = "response")
mu_int[,,3,6]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ RFSD ################
RFSD <- read_csv("Data/RF/RFSD.csv")

#OE-raten for varighed 3 (endepunktet):
RFSD_filtered <- RFSD[RFSD$duration == 3, ]
RFSD_filtered$OE <- RFSD_filtered$O/RFSD_filtered$E
RFSD_OE_endpoint <- sum(RFSD_filtered$O)/sum(RFSD_filtered$E)

#centrerer duration
midpointsduration_RFSD <- c((unique(RFSD$duration)[-1] + unique(RFSD$duration)[-length(unique(RFSD$duration))]) / 2, unique(RFSD$duration)[length(unique(RFSD$duration))])
RFSD$duration <- midpointsduration_RFSD[match(RFSD$duration, unique(RFSD$duration))]

#fjerner sidste datapunkt for duration
RFSD <- RFSD[RFSD$duration != unique(RFSD$duration)[length(unique(RFSD$duration))], ]

RFSD_final <- glm(O~ns(duration,df=3)+offset(log(E)),family = poisson(link="log"),data=RFSD)

#For duration større end 3 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
RFSD_OE_endpoint #0.0006416063

predictions <- predict(RFSD_final, newdata = new_data, type = "response")
mu_int[,,3,1]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ RFJA ################
RFJA <- read_csv("Data/RF/RFJA.csv")

#OE-raten for varighed 1.5 (endepunktet):
RFJA_filtered <- RFJA[RFJA$duration ==  1.5, ]
RFJA_filtered$OE <- RFJA_filtered$O/RFJA_filtered$E
RFJA_OE_endpoint <- sum(RFJA_filtered$O)/sum(RFJA_filtered$E)

#centrerer duration
midpointsduration_RFJA <- c((unique(RFJA$duration)[-1] + unique(RFJA$duration)[-length(unique(RFJA$duration))]) / 2, unique(RFJA$duration)[length(unique(RFJA$duration))])
RFJA$duration <- midpointsduration_RFJA[match(RFJA$duration, unique(RFJA$duration))]

#fjerner sidste datapunkt for duration
RFJA <- RFJA[RFJA$duration != unique(RFJA$duration)[length(unique(RFJA$duration))], ]

RFJA_final <- glm(O ~duration+offset(log(E)),family = poisson(link="log"),data=RFJA)

#For duration større end 1.5 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
RFJA_OE_endpoint #0.0003691616

predictions <- predict(RFJA_final, newdata = new_data, type = "response")
mu_int[,,3,2]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

######################################
####        LEDIGHEDSYDELSE       ####
######################################


################ LYSD ################
LYSD <- read_csv("Data/LY/LYSD.csv")

#OE-raten for varighed 3 (endepunktet):
LYSD_filtered <- LYSD[LYSD$duration == 0.5, ]
LYSD_filtered$OE <- LYSD_filtered$O/LYSD_filtered$E
LYSD_OE_endpoint <- sum(LYSD_filtered$O)/sum(LYSD_filtered$E)

#centrerer duration
midpointsduration_LYSD <- c((unique(LYSD$duration)[-1] + unique(LYSD$duration)[-length(unique(LYSD$duration))]) / 2, unique(LYSD$duration)[length(unique(LYSD$duration))])
LYSD$duration <- midpointsduration_LYSD[match(LYSD$duration, unique(LYSD$duration))]

#fjerner sidste datapunkt for duration
LYSD <- LYSD[LYSD$duration != unique(LYSD$duration)[length(unique(LYSD$duration))], ]

LYSD_final <- glm(O ~ offset(log(E)), family = poisson, data = LYSD)

#For duration større end 0.5 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
LYSD_OE_endpoint #0.0002521938

predictions <- predict(LYSD_final, newdata = new_data, type = "response")
mu_int[,,4,1]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ LYJA ################
LYJA  <- read_csv("Data/LY/LYJA.csv")

view(LYJA)

#OE-raten, for det ene punkt som er:
LYJA_OE_endpoint <- LYJA$O/LYJA$E

LYJA_final <- LYSD_OE_endpoint #0.000264376

predictions <- predict(LYJA_final, newdata = new_data, type = "response")
mu_int[,,4,2]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ LYRF ################
LYRF <- read_csv("Data/LY/LYRF.csv")

#OE-raten for varighed 4 (endepunktet):
LYRF_filtered <- LYRF[LYRF$duration == 4, ]
LYRF_filtered$OE <- LYRF_filtered$O/LYRF_filtered$E
LYRF_OE_endpoint <- sum(LYRF_filtered$O)/sum(LYRF_filtered$E)

#centrerer duration
midpointsduration_LYRF <- c((unique(LYRF$duration)[-1] + unique(LYRF$duration)[-length(unique(LYRF$duration))]) / 2, unique(LYRF$duration)[length(unique(LYRF$duration))])
LYRF$duration <- midpointsduration_LYRF[match(LYRF$duration, unique(LYRF$duration))]

#fjerner sidste datapunkt for duration
LYRF <- LYRF[LYRF$duration != unique(LYRF$duration)[length(unique(LYRF$duration))], ]

LYRF_final <- glm(O ~ poly(duration,2)+offset(log(E)), family = poisson, data = LYRF)

#For duration større end 4 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
LYRF_OE_endpoint #0.002309027

predictions <- predict(LYRF_final, newdata = new_data, type = "response")
mu_int[,,4,3]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ LYFP ################
LYFP <- read_csv("Data/LY/LYFP.csv")
LYFP<-LYFP[-c(1,29,30,31,32),] #Fjerner første alders interval, samt aldre over 67 (har 0 occurences)

#Centrerer age
unique_ages_LYFP <- unique(LYFP$age)
midpoints_LYFP <- (unique_ages_LYFP[-1] + unique_ages_LYFP[-length(unique_ages_LYFP)]) / 2
custom_last_point_LYFP <- (67 + 60) / 2  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_LYFP <- c(midpoints_LYFP, custom_last_point_LYFP)
LYFP$age <- midpoints_LYFP[match(LYFP$age, unique_ages_LYFP)]

LYFP_final<-glm(O~I(age<55) + poly(I(age*(age >= 55)), 3)+offset(log(E)),family = poisson(link="log"),data=LYFP)

predictions <- predict(LYFP_final, newdata = new_data, type = "response")
mu_int[,,4,6]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

######################################
####           FLEKSJOB           ####
######################################

################ FJSD ################
FJSD <- read_csv("Data/FJ/FJSD.csv")

#OE-raten for varighed 5 (endepunktet):
FJSD_filtered <- FJSD[FJSD$duration == 5, ]
FJSD_filtered$OE <- FJSD_filtered$O/FJSD_filtered$E
FJSD_OE_endpoint <- sum(FJSD_filtered$O)/sum(FJSD_filtered$E)

#centrerer duration
midpointsduration_FJSD <- c((unique(FJSD$duration)[-1] + unique(FJSD$duration)[-length(unique(FJSD$duration))]) / 2, unique(FJSD$duration)[length(unique(FJSD$duration))])
FJSD$duration <- midpointsduration_FJSD[match(FJSD$duration, unique(FJSD$duration))]

#fjerner sidste datapunkt for duration
FJSD <- FJSD[FJSD$duration != unique(FJSD$duration)[length(unique(FJSD$duration))], ]

FJSD_final <- glm(O~poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=FJSD)

#For duration større end 5 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
FJSD_OE_endpoint #0.001975652

predictions <- predict(FJSD_final, newdata = new_data, type = "response")
mu_int[,,5,1]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ FJFP ################
FJFP <- read_csv("Data/FJ/FJFP.csv")

#OE-raten for varighed 3 (endepunktet):
FJFP_filtered <- FJFP[FJFP$duration == 3, ]
FJFP_filtered$OE <- FJFP_filtered$O/FJFP_filtered$E
FJFP_OE_endpoint <- sum(FJFP_filtered$O)/sum(FJFP_filtered$E)

#Centrerer age
unique_ages_FJFP <- unique(FJFP$age)
midpoints_FJFP <- (unique_ages_FJFP[-1] + unique_ages_FJFP[-length(unique_ages_FJFP)]) / 2
custom_last_point_FJFP <- (67 + 60) / 2  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FJFP <- c(midpoints_FJFP, custom_last_point_FJFP)
FJFP$age <- midpoints_FJFP[match(FJFP$age, unique_ages_FJFP)]

#centrerer duration
midpointsduration_FJFP <- c((unique(FJFP$duration)[-1] + unique(FJFP$duration)[-length(unique(FJFP$duration))]) / 2, unique(FJFP$duration)[length(unique(FJFP$duration))])
FJFP$duration <- midpointsduration_FJFP[match(FJFP$duration, unique(FJFP$duration))]

#fjerner sidste datapunkt for duration
FJFP <- FJFP[FJFP$duration != unique(FJFP$duration)[length(unique(FJFP$duration))], ]

#fjerner første datapunkt for age, da vi modellere dette seperat
FJFP <- FJFP[FJFP$age != unique(FJFP$age)[1], ]

FJFP_final <- glm(O~poly(age,3)+duration+offset(log(E)),family = poisson(link="log"),data=FJFP)

#For duration større end 5 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
FJFP_OE_endpoint #0.005760366

predictions <- predict(FJFP_final, newdata = new_data, type = "response")
mu_int[,,5,6]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ FJRF ################

FJRF <- read_csv("Data/FJ/FJRF.csv")

#Kun et datapunkt, så sættes konstant
FJRF_final<-sum(FJRF$O)/sum(FJRF$E)

predictions <- predict(FJRF_final, newdata = new_data, type = "response")
mu_int[,,5,3]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ FJJA ################
FJJA <- read_csv("Data/FJ/FJJA.csv")

#OE-raten for varighed 3 (endepunktet):
FJJA_filtered <- FJJA[FJJA$duration == 1.5, ]
FJJA_filtered$OE <- FJJA_filtered$O/FJJA_filtered$E
FJJA_OE_endpoint <- sum(FJJA_filtered$O)/sum(FJJA_filtered$E)

#fjerner sidste datapunkt for duration
FJJA <- FJJA[FJJA$duration != unique(FJJA$duration)[length(unique(FJJA$duration))], ]

FJJA_final<-sum(FJJA$O)/sum(FJJA$E)
#For duration over 1.5
FJJA_OE_endpoint

predictions <- predict(FJJA_final, newdata = new_data, type = "response")
mu_int[,,5,2]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

################ FJLY ################
FJLY <- read_csv("Data/FJ/FJLY.csv")

#OE-raten for varighed 3 (endepunktet):
FJLY_filtered <- FJLY[FJLY$duration == 5, ]
FJLY_filtered$OE <- FJLY_filtered$O/FJLY_filtered$E
FJLY_OE_endpoint <- sum(FJLY_filtered$O)/sum(FJLY_filtered$E)

#Centrerer age
unique_ages_FJLY <- unique(FJLY$age)
midpoints_FJLY <- (unique_ages_FJLY[-1] + unique_ages_FJLY[-length(unique_ages_FJLY)]) / 2
custom_last_point_FJLY <- (67 + 60) / 2  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FJLY <- c(midpoints_FJLY, custom_last_point_FJLY)
FJLY$age <- midpoints_FJLY[match(FJLY$age, unique_ages_FJLY)]

#centrerer duration
midpointsduration_FJLY <- c((unique(FJLY$duration)[-1] + unique(FJLY$duration)[-length(unique(FJLY$duration))]) / 2, unique(FJLY$duration)[length(unique(FJLY$duration))])
FJLY$duration <- midpointsduration_FJLY[match(FJLY$duration, unique(FJLY$duration))]

#fjerner sidste datapunkt for duration
FJLY <- FJLY[FJLY$duration != unique(FJLY$duration)[length(unique(FJLY$duration))], ]

FJLY_final<-glm(O~poly(age,4)+poly(duration,2)+I(duration>=2/12)+offset(log(E)),family = poisson(link="log"),data=FJLY)

#For duration over 5
FJLY_OE_endpoint #0.03170921

predictions <- predict(FJSD_final, newdata = new_data, type = "response")
mu_int[,,5,4]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

######################################
####         FØRTIDSPENSION       ####
######################################


################ FPFJ ################
FPFJ <- read_csv("Data/FP/FPFJ.csv")

#OE-raten for varighed 3 (endepunktet):
FPFJ_filtered <- FPFJ[FPFJ$duration == 3, ]
FPFJ_filtered$OE <- FPFJ_filtered$O/FPFJ_filtered$E
FPFJ_OE_endpoint <- sum(FPFJ_filtered$O)/sum(FPFJ_filtered$E)

#Centrerer age
unique_ages_FPFJ <- unique(FPFJ$age)
midpoints_FPFJ <- (unique_ages_FPFJ[-1] + unique_ages_FPFJ[-length(unique_ages_FPFJ)]) / 2
custom_last_point_FPFJ <- (67 + 60) / 2  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_FPFJ <- c(midpoints_FPFJ, custom_last_point_FPFJ)
FPFJ$age <- midpoints_FPFJ[match(FPFJ$age, unique_ages_FPFJ)]

#centrerer duration
midpointsduration_FPFJ <- c((unique(FPFJ$duration)[-1] + unique(FPFJ$duration)[-length(unique(FPFJ$duration))]) / 2, unique(FPFJ$duration)[length(unique(FPFJ$duration))])
FPFJ$duration <- midpointsduration_FPFJ[match(FPFJ$duration, unique(FPFJ$duration))]

#fjerner sidste datapunkt for duration
FPFJ <- FPFJ[FPFJ$duration != unique(FPFJ$duration)[length(unique(FPFJ$duration))], ]

FPFJ_final<-glm(O~poly(age,4)+poly(duration,2)+offset(log(E)),family = poisson(link="log"),data=FPFJ)

#For duration over 5
FPFJ_OE_endpoint #0.004464665

predictions <- predict(FJJA_final, newdata = new_data, type = "response")
mu_int[,,6,5]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))







