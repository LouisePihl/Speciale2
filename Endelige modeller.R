#Pakker: 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

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


######################################
####         JOBAFKLARING         ####
######################################

################ JARF ################
JARF <- read_csv("Data/JA/JARF.csv")

View(JARF)

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

JARF_final <- #Indsæt model
#For duration større end 4 (svarende til sidste punkt, dette benyttes ikke i modellen) sæt OE-raten konstant til: 
JARF_OE_endpoint #0.03104505

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


################ JAFJ ################
JAFJ <- read_csv("Data/JA/JAFJ.csv")

#OE-raten for varighed 3 (endepunktet) for alle aldre
JAFJ_filtered <- JAFJ[JAFJ$duration == 3, ]
JAFJ_filtered$OE <- JAFJ_filtered$O/JAFJ_filtered$E
JAFJ_OE_endpoint <- sum(JAFJ_filtered$O)/sum(JAFJ_filtered$E)

#Centrerer age
unique_ages_JAFJ <- unique(JAFJ$age)
midpoints_JAFJ <- (unique_ages_JAFJ[-1] + unique_ages_JAFJ[-length(unique_ages_JAFJ)]) / 2
custom_last_point_JAFJ <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JAFJ <- c(midpoints_JAFJ, custom_last_point_JAFJ)
JAFJ$age <- midpoints_JAFJ[match(JAFJ$age, unique_ages_JAFJ)]

#centrerer duration
midpointsduration_JAFJ <- c((unique(JAFJ$duration)[-1] + unique(JAFJ$duration)[-length(unique(JAFJ$duration))]) / 2, unique(JAFJ$duration)[length(unique(JAFJ$duration))])
JAFJ$duration <- midpointsduration_JAFJ[match(JAFJ$duration, unique(JAFJ$duration))]

#fjerner sidste datapunkt for duration
JAFJ <- JAFJ[JAFJ$duration != unique(JAFJ$duration)[length(unique(JAFJ$duration))], ]

JAFJ_final <- #Indsæt model
#For duration større end 3 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JAFJ_OE_endpoint #0.01640263


######################################
####        RESSOURCEFORLØB       ####
######################################


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

################ FJRF ################

FJRF <- read_csv("Data/FJ/FJRF.csv")

#Kun et datapunkt, så sættes konstant
FJRF_final<-sum(FJRF$O)/sum(FJRF$E)

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

######################################
####         FØRTIDSPENSION       ####
######################################

