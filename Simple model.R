##############################################
##         Simple mode - Sampension         ##
##############################################


#------------- Generelle værdier -------------
t_0 <- 41
t_slut <- 67
h <- 1/12  # En måned som trinlængde
w <- 1 #start varrighed

# Antal tidspunkter (325) svarende til månedlige trin fra 40 til 67 år
num_months <- (t_slut-t_0)*1/h+1

# Opret sekvenser for t (alder) og w (varighed)
t_values <- seq(t_0, t_slut, by = h)[1:num_months]  # Værdier af t fra 40 til 67 med trin h
w_values <- seq(w, w+t_slut-t_0, by = h)[1:num_months]  # w ændres på samme måde som t



#------------------ Ydelser ------------------
Benefits_simple <- data.frame(Public_simple = numeric(5),Insurance_simple= numeric(5))
Benefits_simple$Public_simple<-c(18780,0,12330,26830,18490)
Benefits_simple$Insurance_simple<-c(13220,32000,19670,5170,13510)

b <- function(z) {Benefits_simple$Insurance_simple[1]*(z<=5.5/12)+Benefits_simple$Insurance_simple[2]*(5.5/12<z&z<=41.5/12)+Benefits_simple$Insurance_simple[3]*(41.5/12<z&z<=49.5/12)+Benefits_simple$Insurance_simple[4]*(49.5/12<z&z<=109.5/12)+Benefits_simple$Insurance_simple[5]*(109.5/12<z&z<=(t_slut-t_0))}
# i år

#---------------- Intensiteter ----------------
#filling in matrices for disability mortality and reactivation 
mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}

integrand <- function(t){mu_21(t,w)+mu_23(t,w)}

#--------------- Overgangs ssh ----------------

p_22 <- function(t, z) {
  # Funktion der beskriver integranden
  integrand <- function(s){mu_21(s,z+s-t)+mu_23(s,z+s-t)}
  
  # Beregn integralet fra s til t
  integral_result <- integrate(integrand, t, t_slut)
  
  # Beregn p_{22}(t, s, w, z) som eksponentialfunktionen af -integralet
  result <- exp(-integral_result$value)
  
  return(result)
}

#--------------- Cash flow ----------------


# Definer cashflow-funktionen, som inkluderer p_22 og b(w)
cashflow <- function(t,z) {p_22(t,z)*b(z)}

# Beregn cashflow for hver måned
cashflow_values <- mapply(cashflow, t = t_values, z = w_values)


#--------------- Reserve ----------------
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
} #Noget i den her funktion er FORKERT!

# Generér en sekvens af tidspunkter fra 0 til t_slut - t_0 med intervaller på h
s_sequence <- seq(0, t_slut - t_0, h)

# Anvend discount-funktionen på hver værdi i s_sequence
discount_vector <- sapply(s_sequence, discount)

integrand<-cashflow_values*discount_vector
integrand_func<-function(t){integrand[round(t)+1]}
integrate(integrand_func,0,t_slut*12-t_0*12,subdivisions = 1000)
sum(integrand[-length(integrand)])









