library("readxl")
#Reserve regning

#-----------------Sætter offentlige og forsikring ydelser-----------------------
Benefits <- data.frame(Public_1= numeric(6),Insurance_1= numeric(6),Public_2= numeric(6),Insurance_2= numeric(6))

Benefits$Public_1<-c(28780,12330,12330,22330,36830,29540)
Benefits$Insurance_1<-rep(10000,6)
Benefits$Public_2<-c(18780,0,0,12330,26830,18490)
Benefits$Insurance_2<-c(13220,32000,32000,19670,5170,13510)



#-----------------------------Regner cashflows---------------------------------
#Starting in SB
expec_cash_flow_Insurance_1<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Insurance_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_1[1]*exp(-results)
expec_cash_flow_Insurance_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_1[2]*exp(-results)
expec_cash_flow_Insurance_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_1[3]*exp(-results)
expec_cash_flow_Insurance_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_1[4]*exp(-results)
expec_cash_flow_Insurance_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_1[5]*exp(-results)
expec_cash_flow_Insurance_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_1[6]*exp(-results)
expec_cash_flow_Insurance_1$total<-expec_cash_flow_Insurance_1$SB+expec_cash_flow_Insurance_1$JC+expec_cash_flow_Insurance_1$RS+expec_cash_flow_Insurance_1$UB+expec_cash_flow_Insurance_1$FJ+expec_cash_flow_Insurance_1$DP

expec_cash_flow_Public_1<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_1[1]*exp(-results)
expec_cash_flow_Public_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_1[2]*exp(-results)
expec_cash_flow_Public_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_1[3]*exp(-results)
expec_cash_flow_Public_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_1[4]*exp(-results)
expec_cash_flow_Public_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_1[5]*exp(-results)
expec_cash_flow_Public_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_1[6]*exp(-results)
expec_cash_flow_Public_1$total<-expec_cash_flow_Public_1$SB+expec_cash_flow_Public_1$JC+expec_cash_flow_Public_1$RS+expec_cash_flow_Public_1$UB+expec_cash_flow_Public_1$FJ+expec_cash_flow_Public_1$DP

expec_cash_flow_Insurance_2<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Insurance_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_2[1]*exp(-results)
expec_cash_flow_Insurance_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_2[2]*exp(-results)
expec_cash_flow_Insurance_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_2[3]*exp(-results)
expec_cash_flow_Insurance_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_2[4]*exp(-results)
expec_cash_flow_Insurance_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_2[5]*exp(-results)
expec_cash_flow_Insurance_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_2[6]*exp(-results)
expec_cash_flow_Insurance_2$total<-expec_cash_flow_Insurance_2$SB+expec_cash_flow_Insurance_2$JC+expec_cash_flow_Insurance_2$RS+expec_cash_flow_Insurance_2$UB+expec_cash_flow_Insurance_2$FJ+expec_cash_flow_Insurance_2$DP

expec_cash_flow_Public_2<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_2[1]*exp(-results)
expec_cash_flow_Public_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_2[2]*exp(-results)
expec_cash_flow_Public_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_2[3]*exp(-results)
expec_cash_flow_Public_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_2[4]*exp(-results)
expec_cash_flow_Public_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_2[5]*exp(-results)
expec_cash_flow_Public_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_2[6]*exp(-results)
expec_cash_flow_Public_2$total<-expec_cash_flow_Public_2$SB+expec_cash_flow_Public_2$JC+expec_cash_flow_Public_2$RS+expec_cash_flow_Public_2$UB+expec_cash_flow_Public_2$FJ+expec_cash_flow_Public_2$DP


#Plots 
data <- data.frame(
  time = seq(40,67,1/12),
  cf_Insurance1 = expec_cash_flow_Insurance_1$total,
  cf_Insurance2 = expec_cash_flow_Insurance_2$total
)
ggplot(data, aes(x = time, y = cf_Insurance1)) +
  geom_line() +              # Add a line plot
  labs(x = "Time", y = "Expected cash flow")

ggplot(data, aes(x = time, y = cf_Insurance2)) +
  geom_line() +              # Add a line plot
  labs(x = "Time", y = "Expected cash flow")


#-----------------------------------Reserve-------------------------------------
library("readxl")
rentekurve<-read_excel("Rentekurven_020124.xlsx")
years <- 1:120  # Hvis din dataframe har data op til 10 år. Udvid hvis nødvendigt.
rates <- as.numeric(rentekurve[1, 2:(length(years) + 1)])/100 # Ekstraher renteværdierne fra rækken og divider med 100 for at få det i procent

#approximere lineært mellem punkterne i rentekurven 
r <- approxfun(years, rates, method = "linear", rule = 2)

plot(r, xlim=c(0,27))

#Produkt 1
discount <- function(s) {
  integral <- integrate(function(tau) r(tau), lower = 0, upper = s)$value
  return(exp(-integral))
} #Noget i den her funktion er FORKERT!

# Generér en sekvens af tidspunkter fra 0 til t_slut - t_0 med intervaller på h
s_sequence <- seq(0, t_slut - t_0, h)

# Anvend discount-funktionen på hver værdi i s_sequence
discount_vector <- sapply(s_sequence, discount)

sum(integrand[-length(integrand)])
integrand<-expec_cash_flow_Insurance_1$total*discount_vector
integrand_func<-function(t){integrand[round(t)+1]}
integrate(integrand_func,0,t_slut*12-t_0*12,subdivisions = 1000)


#Produkt 2
discount <- function(s) {
  integral <- integrate(function(tau) r(tau), lower = t_0, upper = s + t_0)$value
  return(exp(-integral))
}

# Generér en sekvens af tidspunkter fra 0 til t_slut - t_0 med intervaller på h
s_sequence <- seq(0, t_slut - t_0, h)

# Anvend discount-funktionen på hver værdi i s_sequence
discount_vector <- sapply(s_sequence, discount)

integrand<-expec_cash_flow_Insurance_2$total*discount_vector
integrand_func<-function(t){integrand[round(t/h)+1]}
integrate(integrand_func,0,t_slut-t_0,subdivisions = 1000)



#------------------- IGEN med konstant r til sammenligning ---------------------
r_k<- 0.01
discount<-function(t){exp(-t*r_k)}

integrand<-expec_cash_flow_Insurance_1$total*discount(seq(0,27,h))
integrand_func<-function(t){integrand[round(t/h)+1]}
integrate(integrand_func,0,27,subdivisions = 1000)


integrand<-expec_cash_flow_Insurance_2$total*discount(seq(0,27,h))
integrand_func<-function(t){integrand[round(t/h)+1]}
integrate(integrand_func,0,27,subdivisions = 1000)



