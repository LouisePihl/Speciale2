######################################################
####               PAKKER OG DATA                 ####
######################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(splines)
library(hexbin)

######################################################
####              DATASÆT TIL FP                  ####
######################################################


################ SDFP ################
SDFP<-read_csv("Data/SD/SDFP.csv")

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

SDFP_OE<-sum(SDFP$O)/sum(SDFP$E)


################ JAFP ################
JAFP<-read_csv("Data/JA/JAFP.csv")

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

JAFP_OE<-sum(JAFP$O)/sum(JAFP$E)


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

RFFP_OE<-sum(RFFP$O)/sum(RFFP$E)



################ LYFP ################
LYFP<-read_csv("Data/LY/LYFP.csv")

#Centrerer age
unique_ages_LYFP <- unique(LYFP$age)
midpoints_LYFP <- (unique_ages_LYFP[-1] + unique_ages_LYFP[-length(unique_ages_LYFP)]) / 2
custom_last_point_LYFP <- (67 + 60) / 2  # Brug 63.5 som værdi i højre endepunkt da det sidste datapunkt er 60 år
midpoints_LYFP <- c(midpoints_LYFP, custom_last_point_LYFP)
LYFP$age <- midpoints_LYFP[match(LYFP$age, unique_ages_LYFP)]

LYFP_OE<-sum(LYFP$O)/sum(LYFP$E) #KUN ALDER
LYFP$OE <- LYFP$O / LYFP$E



################ FJFP ################
FJFP<-read_csv("Data/FJ/FJFP.csv")
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

FJFP_OE<-sum(FJFP$O)/sum(FJFP$E)


######################################################
####      AGGREGERING OG IKKE-LINEARE TRENDS      ####
######################################################

# Aggregering af data efter 'age'
AgeAgg_SDFP <- SDFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_JAFP <- JAFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_RFFP <- RFFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_FJFP <- FJFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)

# Aggregering af data efter 'age'
AgeAgg_LYFP <- LYFP %>%
  group_by(age) %>%
  summarise(expoAgg = sum(E), occAgg=sum(O)) %>%
  mutate(OE = occAgg/expoAgg)


# Plots af trends
plot(AgeAgg_SDFP$age, AgeAgg_SDFP$OE)
plot(AgeAgg_JAFP$age, AgeAgg_JAFP$OE)
plot(AgeAgg_RFFP$age, AgeAgg_RFFP$OE)
plot(LYFP$age, LYFP$OE)
plot(AgeAgg_FJFP$age, AgeAgg_FJFP$OE)

?plot

# Opret første plot med linjer
plot(AgeAgg_SDFP$age, AgeAgg_SDFP$OE, type = "p", col = "blue", lwd = 2.5, 
     xlab = "Age", ylab = "OE", ylim=c(0,0.09), xlim=c(18,70))

# Tilføj yderligere linjer til plottet
lines(AgeAgg_JAFP$age, AgeAgg_JAFP$OE, col = "aquamarine3", lwd = 2.5)
lines(AgeAgg_RFFP$age, AgeAgg_RFFP$OE, col = "forestgreen", lwd = 2.5)
lines(AgeAgg_LYFP$age, AgeAgg_LYFP$OE, col = "steelblue", lwd = 2.5)
lines(AgeAgg_FJFP$age, AgeAgg_FJFP$OE, col = "darkseagreen4", lwd = 2.5)

# Tilføj en legende for at identificere de forskellige grupper
legend("topleft", legend = c("SDFP", "JAFP", "RFFP", "LYFP", "FJFP"), 
       col = c("blue", "aquamarine3", "forestgreen", "steelblue", "darkseagreen4"), lwd = 3)
abline(v = 40, col = "black", lty = 2)





# Opret første plot med punkter
plot(AgeAgg_SDFP$age, AgeAgg_SDFP$OE, type = "p", col = "blue", pch = 16, cex = 1.5,
     xlab = "Age", ylab = "OE", ylim = c(0, 0.09), xlim = c(18, 70))

# Tilføj yderligere punkter til plottet
points(AgeAgg_JAFP$age, AgeAgg_JAFP$OE, col = "aquamarine3", pch = 16, cex = 1.5)
points(AgeAgg_RFFP$age, AgeAgg_RFFP$OE, col = "forestgreen", pch = 16, cex = 1.5)
points(AgeAgg_LYFP$age, AgeAgg_LYFP$OE, col = "steelblue", pch = 16, cex = 1.5)
points(AgeAgg_FJFP$age, AgeAgg_FJFP$OE, col = "darkseagreen4", pch = 16, cex = 1.5)

# Tilføj en legende for at identificere de forskellige grupper
legend("topleft", legend = c("SDFP", "JAFP", "RFFP", "LYFP", "FJFP"), 
       col = c("blue", "aquamarine3", "forestgreen", "steelblue", "darkseagreen4"), pch = 16)

# Tilføj en lodret linje ved x = 40
abline(v = 40, col = "black", lty = 2)


# Opret første plot med punkter
plot(AgeAgg_LYFP$age, AgeAgg_LYFP$OE, type = "p", col = "purple", pch = 16, cex = 1.5,
     xlab = "Age", ylab = "OE", ylim = c(0, 0.09), xlim = c(18, 70))

# Tilføj yderligere punkter til plottet
points(AgeAgg_JAFP$age, AgeAgg_JAFP$OE, col = "darkorange", pch = 16, cex = 1.5)
points(AgeAgg_RFFP$age, AgeAgg_RFFP$OE, col = "darkgreen", pch = 16, cex = 1.5)
points(AgeAgg_SDFP$age, AgeAgg_SDFP$OE, col = "red", pch = 16, cex = 1.5)
points(AgeAgg_FJFP$age, AgeAgg_FJFP$OE, col = "dodgerblue", pch = 16, cex = 1.5)

# Tilføj en legende for at identificere de forskellige grupper
legend("topleft", legend = c("SDFP", "JAFP", "RFFP", "LYFP", "FJFP"), 
       col = c("red", "darkorange", "darkgreen", "purple", "dodgerblue"), pch = 16)

# Tilføj en lodret linje ved x = 40
abline(v = 40, col = "black", lty = 2)






#----------- Ny ---------------
# Indlæs nødvendige pakker
library(ggplot2)
library(dplyr)

# Opret dataramme for AgeAgg_SDFP, AgeAgg_JAFP, AgeAgg_RFFP, AgeAgg_LYFP og AgeAgg_FJFP
df <- bind_rows(
  AgeAgg_SDFP %>% mutate(Group = "SBDP"),
  AgeAgg_JAFP %>% mutate(Group = "JCDP"),
  AgeAgg_RFFP %>% mutate(Group = "RSDP"),
  AgeAgg_LYFP %>% mutate(Group = "UBDP"),
  AgeAgg_FJFP %>% mutate(Group = "FJDP")
)

# Lav plottet
plot_OE <- ggplot(df, aes(x = age, y = OE, color = Group)) +
  geom_point(size = 3) +  # Plot prikkerne
  scale_color_manual(values = c("SDFP" = "blue", "JAFP" = "aquamarine3", "RFFP" = "forestgreen", 
                                "LYFP" = "steelblue", "FJFP" = "darkseagreen4")) +  # Farver for grupperne
  labs(
    x = "Age",
    y = "OE",
    title = ""
  ) +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "topleft",  # Placering af legenden
    legend.title = element_blank()  # Fjern legendens titel
  ) +
  geom_vline(xintercept = 40, color = "black", linetype = "dashed", size = 1)  # Tilføj lodret linje

# Print plottet
print(plot_OE)





# Lav plottet
plot_OE <- ggplot(df, aes(x = age, y = OE, color = Group)) +
  geom_point(size = 3) +  # Plot prikkerne
  scale_color_manual(values = c("SBDP" = "#1f77b4",   # Stærk blå
                                "JCDP" = "#7ec8e3",   # Lys blå
                                "RSDP" = "#2ca02c",   # Mørk grøn
                                "UBDP" = "#98c9a3",   # Lysere grøn
                                "FJDP" = "royalblue4")) +  # Mellemgrøn farve
  labs(
    x = "Age",
    y = "OE",
    title = ""
  ) +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = c(0.1, 0.9),  # Placering af legenden i venstre øverste hjørne
    legend.title = element_blank()  # Fjern legendens titel
  ) +
  geom_vline(xintercept = 40, color = "black", linetype = "dashed", size = 1)  # Tilføj lodret linje

# Print plottet
print(plot_OE)


# Lav plottet
plot_OE <- ggplot(df, aes(x = age, y = OE, color = Group)) +
  geom_point(size = 3) +  # Plot prikkerne
  scale_color_manual(values = c("SDFP" = "black",   # Stærk blå
                                "JAFP" = "olivedrab3",   # Lys blå
                                "RFFP" = "seagreen",   # Mørk grøn
                                "LYFP" = "#7ec8e3",   # Lysere grøn
                                "FJFP" = "royalblue4")) +  # Mellemgrøn farve
  labs(
    x = "Age",
    y = "OE rate",
    title = ""
  ) +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = c(0.2, 0.75),  # Placering af legenden længere nede
    legend.title = element_blank()  # Fjern legendens titel
  ) +
  geom_vline(xintercept = 40, color = "black", linetype = "dashed", size = 1)  # Tilføj lodret linje

# Print plottet
print(plot_OE)

ggsave("DP_points.png", plot = plot_OE, width = 6, height = 4, dpi = 300)

