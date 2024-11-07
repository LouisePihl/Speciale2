##############################################
##         Simple mode - Sampension         ##
##############################################


#------------- Generelle værdier -------------
t_0<-40
t_slut<-67
u_0<-0
u_slut<-t_slut-t_0
h<-1/12
N_time<-round((t_slut-t_0)/h)
N_duration<-round((u_slut-u_0)/h)
#mu_p_int<-array(0,c(N_time+1,N_duration+1,6)) 


#------------------ Ydelser ------------------
Benefits_simple <- data.frame(Public_simple = numeric(5),Insurance_simple= numeric(5))
Benefits_simple$Public_simple<-c(18780,0,12330,26830,18490)
Benefits_simple$Insurance_simple<-c(13220,32000,19670,5170,13510)

b <- function(z) {Benefits_simple$Insurance_simple[1]*(z<=5.5)+Benefits_simple$Insurance_simple[2]*(5.5<z&z<=41.5)+Benefits_simple$Insurance_simple[3]*(41.5<z&z<=49.5)+Benefits_simple$Insurance_simple[4]*(49.5<z&z<=109.5)+Benefits_simple$Insurance_simple[5]*(109.5<z&z<=(t_slut-t_0)*12)}


#---------------- Intensiteter ----------------
#filling in matrices for disability mortality and reactivation 
mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}


#---------------- Kolmogorov ----------------







