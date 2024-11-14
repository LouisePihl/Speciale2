##############################################
##                 Priser                   ##
##############################################

t_0 <- 40
t_slut <- 67
h <- 1/12  # En måned som trinlængde
w <- 0 #start varrighed
eps <- 1 #dækningsperioden


#------------- Intensiteter ------------------
library("readxl")
levetidbench <- read_excel("Benchmark_doedelighed2022_010224.xlsx")
mu_13 <- function (t){levetidbench$Kvinder[t+1]}

mu_12 <-function(t,z){0.01034251*(t>67)+exp(-22.93861+0.8512794751*t+0.007038620628*t^2-0.001014142721*t^3+1.910555732* 10^(-5)*t^4-1.112835873* 10^(-7)*t^5)*(t<=67)}

mu_12_m <-function(t,z){0.0009687435*(t>67) +(exp(72.53851-10.66927*t+0.53371*t^{2}-0.012798*t^3+1.4922*10^{-4}*t^{4}-6.8007*10^{-7}*t^5))*(t<=67)}


plot(mu_13,xlim=c(30,60))
plot(mu_12,xlim=c(20,67))
plot(mu_12_m,xlim=c(20,67))

#tjek lige om det er for mænd

reserve_DI <- sum(integrand2[1:12])
reserve_DI_simple <- sum(integrand[1:12])

#---------------- mu_ACDI --------------------

t_values <- seq(t_0, t_0+eps, by = h)

mu_ACDI_vector <- sapply(t_values, mu_12)
#mu_ACDE_vector <- sapply(t_values, mu_13)


#------------- exp(-int mu)-------------------
int <- function(t) {
  mu_12(t)+mu_13(t)
}

# Opret en sekvens af t og z værdier med trin h = 1/12 (månedlige trin)
t_values <- seq(t_0, t_0+eps, by = h)

# Initialiser en vektor til at gemme resultaterne og den akkumulerede sum
results <- numeric(length(t_values))
accumulated_sum <- 0

# Start første trin ved at evaluere ved første t og z
t_lower <- t_values[1]

# Loop over de resterende værdier af t og z, og akkumuler kun det nye inkrement
for (i in 2:length(t_values)) {
  t_upper <- t_values[i]
  
  # Integrer kun over det lille interval fra forrige til nuværende step
  increment <- int(t_upper) * h
  
  # Læg inkrementet til den akkumulerede sum
  accumulated_sum <- accumulated_sum + increment
  results[i] <- accumulated_sum
}

p_12_vektor <- exp(-results)

#------------- exp(-int r)-------------------
library("readxl")
rentekurve<-read_excel("Rentekurven_020124.xlsx")
years <- 1:120  # Hvis din dataframe har data op til 10 år. Udvid hvis nødvendigt.
rates <- as.numeric(rentekurve[1, 2:(length(years) + 1)])/100 # Ekstraher renteværdierne fra rækken og divider med 100 for at få det i procent

#approximere lineært mellem punkterne i rentekurven 
r <- approxfun(years, rates, method = "linear", rule = 2)

#Simple produkt
discount <- function(s) {
  integral <- integrate(function(tau) r(tau), lower = 0, upper = s)$value
  return(exp(-integral))
} 

# Generér en sekvens af tidspunkter fra 0 til t_slut - t_0 med intervaller på h
s_sequence <- seq(0, eps, h)

# Anvend discount-funktionen på hver værdi i s_sequence
discount_vector <- sapply(s_sequence, discount)



#------------- Priser -------------------
sum(discount_vector*p_12_vektor*reserve_DI*mu_ACDI_vector)

sum(discount_vector[1:12]*p_12_vektor[1:12]*sum(integrand2)*mu_ACDI_vector[1:12]/12)

sum(integrand2)*mu_12(40,0)
