library("readxl")
#Reserve regning
setwd("/Users/frejalundfredholm/Desktop/Speciale/Speciale2")
#-----------------Sætter offentlige og forsikring ydelser-----------------------
Benefits <- data.frame(Public_1= numeric(6),Insurance_1= numeric(6),Public_2= numeric(6),Insurance_2= numeric(6))

Benefits$Public_1<-c(18780,12330,12330,22330,36830,29540)
Benefits$Insurance_1<-rep(13220,6)
Benefits$Public_2<-c(18780,0,0,12330,26830,18490)*0.95
#Benefits$Insurance_2<-c(13220,32000,32000,19670,5170,13510)
Benefits$Insurance_2<-c(14159,32000,32000,20287,6164,14966)
#Benefits$Insurance_2<-c(13220,32000,32000,19670,13668,13510)

Benefits$Public_2+Benefits$Insurance_2

#-----------------------------Regner cashflows---------------------------------
#Starting in SB
expec_cash_flow_Insurance_1<-data.frame(SB=numeric(N_duration+1-u/h))
expec_cash_flow_Insurance_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_1[1]*exp(-results)
expec_cash_flow_Insurance_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_1[2]*exp(-results)
expec_cash_flow_Insurance_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_1[3]*exp(-results)
expec_cash_flow_Insurance_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_1[4]*exp(-results)
expec_cash_flow_Insurance_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_1[5]*exp(-results)
expec_cash_flow_Insurance_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_1[6]*exp(-results)
expec_cash_flow_Insurance_1$total<-expec_cash_flow_Insurance_1$SB+expec_cash_flow_Insurance_1$JC+expec_cash_flow_Insurance_1$RS+expec_cash_flow_Insurance_1$UB+expec_cash_flow_Insurance_1$FJ+expec_cash_flow_Insurance_1$DP

expec_cash_flow_Insurance_2<-data.frame(SB=numeric(N_duration+1-u/h))
expec_cash_flow_Insurance_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_2[1]*exp(-results)
expec_cash_flow_Insurance_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_2[2]*exp(-results)
expec_cash_flow_Insurance_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_2[3]*exp(-results)
expec_cash_flow_Insurance_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_2[4]*exp(-results)
expec_cash_flow_Insurance_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_2[5]*exp(-results)
expec_cash_flow_Insurance_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_2[6]*exp(-results)
expec_cash_flow_Insurance_2$total<-expec_cash_flow_Insurance_2$SB+expec_cash_flow_Insurance_2$JC+expec_cash_flow_Insurance_2$RS+expec_cash_flow_Insurance_2$UB+expec_cash_flow_Insurance_2$FJ+expec_cash_flow_Insurance_2$DP

#integrand2<-expec_cash_flow_Insurance_2$total*discount_vector
#sum(integrand2[-length(integrand2)])

#integrand1<-expec_cash_flow_Insurance_1$total*discount_vector
#sum(integrand1[-length(integrand1)])

#expec_cash_flow_Public_1<-data.frame(SB=numeric(N_duration+1))
#expec_cash_flow_Public_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_1[1]*exp(-results)
#expec_cash_flow_Public_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_1[2]*exp(-results)
#expec_cash_flow_Public_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_1[3]*exp(-results)
#expec_cash_flow_Public_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_1[4]*exp(-results)
#expec_cash_flow_Public_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_1[5]*exp(-results)
#expec_cash_flow_Public_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_1[6]*exp(-results)
#expec_cash_flow_Public_1$total<-expec_cash_flow_Public_1$SB+expec_cash_flow_Public_1$JC+expec_cash_flow_Public_1$RS+expec_cash_flow_Public_1$UB+expec_cash_flow_Public_1$FJ+expec_cash_flow_Public_1$DP
#
#expec_cash_flow_Public_2<-data.frame(SB=numeric(N_duration+1))
#expec_cash_flow_Public_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_2[1]*exp(-results)
#expec_cash_flow_Public_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_2[2]*exp(-results)
#expec_cash_flow_Public_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_2[3]*exp(-results)
#expec_cash_flow_Public_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_2[4]*exp(-results)
#expec_cash_flow_Public_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_2[5]*exp(-results)
#expec_cash_flow_Public_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_2[6]*exp(-results)
#expec_cash_flow_Public_2$total<-expec_cash_flow_Public_2$SB+expec_cash_flow_Public_2$JC+expec_cash_flow_Public_2$RS+expec_cash_flow_Public_2$UB+expec_cash_flow_Public_2$FJ+expec_cash_flow_Public_2$DP


#Plots 
data_stress <- data.frame(
 time = seq(40,67,h),
 cf_Insurance1_stress = expec_cash_flow_Insurance_1$total,
 cf_Insurance2_stress = expec_cash_flow_Insurance_2$total
)

#data_ustress <- data.frame(
 #time = seq(40,67,h),
 #cf_Insurance1 = expec_cash_flow_Insurance_1$total,
 #cf_Insurance2 = expec_cash_flow_Insurance_2$total
#)

data_ustress$scenario <- "Base"
data_stress$scenario <- "Scenario 4"


p <- ggplot() +
  # Base scenarie
  geom_line(data = data_ustress, aes(x = time, y = cf_Insurance2, color = "Base"), size = 1.1, linetype = "solid") +
  # Mortality stress scenarie
  geom_line(data = data_stress, aes(x = time, y = cf_Insurance2_stress, color = "Scenario 4"), size = 1.1, linetype = "dashed") +
  # Labels og tema
  labs(x = "Age", y = "Expected cash flow") +  # Fjern legend titel
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),  # Flyt legend ind i grafen (x, y koordinater)  # Tilføj baggrund til legenden
    legend.key = element_blank(),  # Fjern nøgle-rammer
    legend.title = element_blank(),  # Fjern legend titel
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  ) +
  # Specificer farver for scenarierne
  scale_color_manual(values = c("Base" = "steelblue", "Scenario 4" = "aquamarine3")) 

print(p)

# Gem plot
ggsave("cf_Insurance2_comparison_scenario4.png", plot = p, width = 6, height = 3, dpi = 300)



p <- ggplot() +
  # Base scenarie
  geom_line(data = data_ustress, aes(x = time, y = cf_Insurance1, color = "Base"), size = 1.1, linetype = "solid") +
  # Mortality stress scenarie
  geom_line(data = data_stress, aes(x = time, y = cf_Insurance1_stress, color = "Scenario 1"), size = 1.1, linetype = "dashed") +
  # Labels og tema
  labs(x = "Age", y = "Expected cash flow") +  # Fjern legend titel
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),  # Flyt legend ind i grafen (x, y koordinater)  # Tilføj baggrund til legenden
    legend.key = element_blank(),  # Fjern nøgle-rammer
    legend.title = element_blank(),  # Fjern legend titel
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  ) +
  # Specificer farver for scenarierne
  scale_color_manual(values = c("Base" = "steelblue", "Scenario 1" = "aquamarine3")) 

print(p)

# Gem plot
ggsave("cf_Insurance1_comparison_scenario1.png", plot = p, width = 6, height = 3, dpi = 300)



#-----------------------------------Renter-------------------------------------
library("readxl")
rentekurve<-read_excel("Rentekurven_020124.xlsx")
years <- 1:120  # Hvis din dataframe har data op til 10 år. Udvid hvis nødvendigt.
rates <- as.numeric(rentekurve[1, 2:(length(years) + 1)])/100 # Ekstraher renteværdierne fra rækken og divider med 100 for at få det i procent

#approximere lineært mellem punkterne i rentekurven 
r <- approxfun(years, rates, method = "linear", rule = 2)

plot_data <- data.frame(years = seq(0, 30, by = 0.1), rates = r(seq(0, 30, by = 0.1)))


# Plot af renten med forbedret layout
p <- ggplot(plot_data, aes(x = years, y = rates)) +
  geom_line(color = "blue", size = 1) +  # Sort stiplet linje
  labs(x = "Years", y = "Interest rate") +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  ) +
  annotate("text", 
           x = mean(plot_data$years), y = max(plot_data$rates) * 0.8,  # Juster annoteringsposition
           label = "", 
           color = "black", size = 6, family = "Times New Roman")  # Tekst-annotering

print(p)




# Løbende år
lobetid <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
             11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 90)

# Fald i procent (ændret til decimalform for beregning)
fald <- c(0.75, 0.65, 0.56, 0.50, 0.46, 0.42, 0.39, 0.36, 0.33, 0.31, 
          0.30, 0.29, 0.28, 0.28, 0.27, 0.28, 0.28, 0.28, 0.29, 0.29, 0.20)

# Funktion til interpolation af fald
r_fald <- approxfun(lobetid, fald, method = "linear", rule = 2)

rentekurve<-read_excel("Rentekurven_020124.xlsx")
years <- 1:120  # Hvis din dataframe har data op til 10 år. Udvid hvis nødvendigt.
rates <- as.numeric(rentekurve[1, 2:(length(years) + 1)])/100 # Ekstraher renteværdierne fra rækken og divider med 100 for at få det i procent

#approximere lineært mellem punkterne i rentekurven 
r <- approxfun(years, rates, method = "linear", rule = 2)

plot_data <- data.frame(years = seq(0, 30, by = 0.1), rates = r(seq(0, 30, by = 0.1)))


# Kombineret rentekurve med fald
justeret_rentekurve <- function(x) r(x) * r_fald(x)

# Opret plot-data
plot_data <- data.frame(
  years = seq(1, 30, by = 0.1),  # År fra 1 til 30 med 0.1 interval
  rates = justeret_rentekurve(seq(1, 30, by = 0.1))  # Kombineret rente
)

# Plot af rentekurve med forbedret layout
p_fald <- ggplot(plot_data, aes(x = years, y = rates)) +
  geom_line(color = "steelblue", size = 1) +  # Blå linje
  labs(x = "Years", y = "Interest rate") +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  )

print(p_fald)

ggsave("r_stress_fald.png", plot = p_fald, width = 6, height = 3, dpi = 300)


# Stigning i procent (ændret til decimalform for beregning)
stigning <- c(1.70, 1.70, 1.64, 1.59, 1.55, 1.52, 1.49, 1.47, 1.44, 1.42, 
              1.39, 1.37, 1.35, 1.34, 1.33, 1.31, 1.30, 1.29, 1.27, 1.26, 1.20) 

# Funktion til interpolation af stigning
r_stigning <- approxfun(lobetid, stigning, method = "linear", rule = 2)

rentekurve<-read_excel("Rentekurven_020124.xlsx")
years <- 1:120  # Hvis din dataframe har data op til 10 år. Udvid hvis nødvendigt.
rates <- as.numeric(rentekurve[1, 2:(length(years) + 1)])/100 # Ekstraher renteværdierne fra rækken og divider med 100 for at få det i procent

#approximere lineært mellem punkterne i rentekurven 
r <- approxfun(years, rates, method = "linear", rule = 2)

plot_data <- data.frame(years = seq(0, 30, by = 0.1), rates = r(seq(0, 30, by = 0.1)))


# Opret en ny rentekurve med stigning anvendt
justeret_rentekurve_stigning <- function(x) r(x) * r_stigning(x)

# Opret plot-data for den justerede rentekurve med stigning
plot_data_stigning <- data.frame(
  years = seq(1, 30, by = 0.1),  # År fra 1 til 30 med 0.1 interval
  rates = justeret_rentekurve_stigning(seq(1, 30, by = 0.1))  # Kombineret rente
)

# Plot af rentekurve med stigning
p_stigning <- ggplot(plot_data_stigning, aes(x = years, y = rates)) +
  geom_line(color = "cornflowerblue", size = 1) +  # Grøn linje
  labs(x = "Years", y = "Interest rate") +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  ) + 
  xlim(c(24, 29)) +  # Definer x-aksens interval
  ylim(c(0.0325, 0.035))

print(p_stigning)

ggsave("r_stress_stigning.png", plot = p_stigning, width = 6, height = 3, dpi = 300)



# Kombineret datasæt til alle rentekurver
combined_data <- data.frame(
  years = seq(1, 30, by = 0.1),
  rates_original = r(seq(1, 30, by = 0.1)),
  rates_fald = justeret_rentekurve(seq(1, 30, by = 0.1)),
  rates_stigning = justeret_rentekurve_stigning(seq(1, 30, by = 0.1))
)

# Transformér data til lang form for nemt at plotte flere kurver
library(tidyr)
plot_data_long <- pivot_longer(
  combined_data,
  cols = c(rates_original, rates_fald, rates_stigning),
  names_to = "curve_type",
  values_to = "rates"
)

combined_plot <- ggplot(plot_data_long, aes(x = years, y = rates, color = curve_type)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("rates_original" = "blue", "rates_fald" = "hotpink2", "rates_stigning" = "hotpink"),
    labels = c("With increase", "Original", "With decline")
  ) +
  labs(x = "Years", y = "Interest rate") +
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),  # Flyt legend ind i grafen (x, y koordinater)
    legend.key = element_blank(),  # Fjern nøgle-rammer
    legend.title = element_blank(),  # Fjern overskriften på legenden
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  )

print(combined_plot)

# Gem plot som billede
ggsave("combined_r_plot.png", plot = combined_plot, width = 8, height = 6, dpi = 300)





#-------------Reserver-----------

#Produkt 1
discount <- function(s) {
  integral <- integrate(function(tau) justeret_rentekurve(tau), lower = 0, upper = s)$value
  return(exp(-integral))
} 

discount <- function(s) {
  integral <- integrate(function(tau) r(tau), lower = 0, upper = s)$value
  return(exp(-integral))
} 


discount <- function(s) {
  tryCatch({
    integral <- integrate(function(tau) justeret_rentekurve_stigning(tau), lower = 0, upper = s)$value
    return(exp(-integral))
  }, error = function(e) {
    # Hvis en fejl opstår, returner en standardværdi (f.eks. returnere 1 for diskonteringsfaktor)
    return(0.3666)
  })
}


# Plot de diskonteringsfaktorer mod tid
plot(s_sequence,discount_vector, type = "l", col = "blue", 
     xlab = "Time (s)", ylab = "Discount factor", 
     main = "Discount Factor vs Time", lwd = 2)

s_sequence <- seq(0, t_slut-t_0, 1/12)  # Kortere tidsinterval til fejlsøgning
discount_vector <- sapply(s_sequence, discount)

integrand1<-expec_cash_flow_Insurance_1$total*discount_vector
sum(integrand1[-length(integrand1)])
integrand2<-expec_cash_flow_Insurance_2$total*discount_vector
sum(integrand2[-length(integrand2)])


