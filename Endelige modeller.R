
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

SDJA_final <- glm(O ~ age + poly(duration, 4) + I(duration >= 2/12), offset = log(E), family = poisson, data = SDJA)

#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
SDJA_OE_endpoint



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

SDRF_final <- #Indsæt model
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
SDRF_OE_endpoint



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

SDLY_final <- #Indsæt model
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
SDLY_OE_endpoint



################ SDFJ ################
SDFJ <- read_csv("Data/SD/SDFJ.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDFJ_filtered <- SDFJ[SDFJ$duration == 3, ]
SDFJ_filtered$OE <- SDFJ_filtered$O/SDFJ_filtered$E
SDFJ_OE_endpoint<-sum(SDFJ_filtered$O)/sum(SDFJ_filtered$E)


#Centrerer age
unique_ages_SDFJ <- unique(SDFJ$age)
midpoints_SDFJ <- (unique_ages_SDFJ[-1] + unique_ages_SDFJ[-length(unique_ages_SDFJ)]) / 2
custom_last_point_SDFJ <- (67 - 59) / 2 + 59  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDFJ <- c(midpoints_SDFJ, custom_last_point_SDFJ)
SDFJ$age <- midpoints_SDFJ[match(SDFJ$age, unique_ages_SDFJ)]

#centrerer duration
midpointsduration_SDFJ<-c((unique(SDFJ$duration)[-1]+unique(SDFJ$duration)[-length(unique(SDFJ$duration))])/2,unique(SDFJ$duration)[length(unique(SDFJ$duration))])
SDFJ$duration<-midpointsduration_SDFJ[match(SDFJ$duration, unique(SDFJ$duration))]

#fjerner sidste datapunkt for duration
SDFJ<-SDFJ[SDFJ$duration!=unique(SDFJ$duration)[length(unique(SDFJ$duration))],]

SDFJ_final <- #Indsæt model
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
SDFJ_OE_endpoint


################ SDFP ################
SDFP <- read_csv("Data/SD/SDFP.csv")

#OE-raten for varrighed 3 (endepunktet) for alle aldre
SDFP_filtered <- SDFP[SDFP$duration == 3, ]
SDFP_filtered$OE <- SDFP_filtered$O/SDFP_filtered$E
SDFP_OE_endpoint<-sum(SDFP_filtered$O)/sum(SDFP_filtered$E)


#Centrerer age
unique_ages_SDFP <- unique(SDFP$age)
midpoints_SDFP <- (unique_ages_SDFP[-1] + unique_ages_SDFP[-length(unique_ages_SDFP)]) / 2
custom_last_point_SDFP <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_SDFP <- c(midpoints_SDFP, custom_last_point_SDFP)
SDFP$age <- midpoints_SDFP[match(SDFP$age, unique_ages_SDFP)]

#centrerer duration
midpointsduration_SDFP<-c((unique(SDFP$duration)[-1]+unique(SDFP$duration)[-length(unique(SDFP$duration))])/2,unique(SDFP$duration)[length(unique(SDFP$duration))])
SDFP$duration<-midpointsduration_SDFP[match(SDFP$duration, unique(SDFP$duration))]

#fjerner sidste datapunkt for duration
SDFP<-SDFP[SDFP$duration!=unique(SDFP$duration)[length(unique(SDFP$duration))],]

SDFP_final <- #Indsæt model
  #For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
SDFP_OE_endpoint


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
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JARF_OE_endpoint

################ JALY ################
JALY <- read_csv("Data/JA/JALY.csv")

#OE-raten for varighed 3 (endepunktet) for alle aldre
JALY_filtered <- JALY[JALY$duration == 4.5, ]
JALY_filtered$OE <- JALY_filtered$O/JALY_filtered$E
JALY_OE_endpoint <- sum(JALY_filtered$O)/sum(JALY_filtered$E)

#Centrerer age
unique_ages_JALY <- unique(JALY$age)
midpoints_JALY <- (unique_ages_JALY[-1] + unique_ages_JALY[-length(unique_ages_JALY)]) / 2
custom_last_point_JALY <- (67 - 60) / 2 + 60  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_JALY <- c(midpoints_JALY, custom_last_point_JALY)
JALY$age <- midpoints_JALY[match(JALY$age, unique_ages_JALY)]

#centrerer duration
midpointsduration_JALY <- c((unique(JALY$duration)[-1] + unique(JALY$duration)[-length(unique(JALY$duration))]) / 2, unique(JALY$duration)[length(unique(JALY$duration))])
JALY$duration <- midpointsduration_JALY[match(JALY$duration, unique(JALY$duration))]

#fjerner sidste datapunkt for duration
JALY <- JALY[JALY$duration != unique(JALY$duration)[length(unique(JALY$duration))], ]

JALY_final <- #Indsæt model
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JALY_OE_endpoint



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
#For duration større end 2.875 (svarende til andet sidste punkt/sidste punkt som benyttes i modellen) sæt OE-raten konstant til: 
JAFJ_OE_endpoint




######################################
####        RESSOURCEFORLØB       ####
######################################





######################################
####        LEDIGHEDSYDELSE       ####
######################################




######################################
####           FLEKSJOB           ####
######################################




######################################
####         FØRTIDSPENSION       ####
######################################

