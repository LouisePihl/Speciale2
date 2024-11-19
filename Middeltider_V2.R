##############################################
##               Middeltider                ##
##############################################

#---------------- Ændre overgange og tal ---------------- 
#Ændre her for at ændre overgangen: 
i <- 4

t_0<-40
t_slut<-67
h<-1/12
N_time<-round((t_slut-t_0)/h)

#mu i,j \in J^I
u_0<-0
u_slut<-t_slut-t_0
N_duration<-round((u_slut-u_0)/h)
useq<-seq(u_0,67-t_0+u_0,1/12)

#mu j \in DE,RA
xseq<-seq(t_0,67,1/12)
w <- 1 #varrighed siden sygdom 
wseq<-seq(w,67-t_0+w,1/12)
tseq<-seq(0,67-t_0,1/12)

#---------------- Trapez regel for alm. integraler ---------------- 
trapezoidal_integration_cum <- function(x, y) {
  # Number of intervals
  n <- length(x) - 1
  # Initialize vector to store cumulative sums
  cumulative_sums <- numeric(n)
  # Initialize the integration result
  integration_result <- 0
  # Loop through each interval and calculate trapezoidal areas
  for (i in 1:n) {
    # Calculate the area of the trapezoid
    area <- (y[i] + y[i + 1]) / 2 * (x[i + 1] - x[i])
    # Add to the cumulative sum
    integration_result <- integration_result + area
    # Store the cumulative sum up to the current interval
    cumulative_sums[i] <- integration_result
  }
  return(cumulative_sums)
}

trapezoidal_integration <- function(x, y) {
  # Number of intervals
  n <- length(x) - 1
  # Initialize vector to store cumulative sums
  cumulative_sums <- numeric(n)
  # Initialize the integration result
  integration_result <- 0
  # Loop through each interval and calculate trapezoidal areas
  for (i in 1:n) {
    # Calculate the area of the trapezoid
    area <- (y[i] + y[i + 1]) / 2 * (x[i + 1] - x[i])
    # Add to the cumulative sum
    integration_result <- integration_result + area
    # Store the cumulative sum up to the current interval
    cumulative_sums[i] <- integration_result
  }
  return(integration_result)
}
#---------------- Intensiteter ud af stadie i---------------- 

#Intensiteter inden i invalide stadiet: 
#mu_matrix <- outer(xseq, useq, Vectorize(function(t, u) mu_p(i, t, u)))

mu_vec <- Vectorize(mu_p, vectorize.args = c("t", "u"))


#---------------- Intensiteter ud af stadie i---------------- 
mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}
mu_2p<-function(t,z){mu_21(t,z)+mu_23(t,z)}
mu2p_vec <- Vectorize(mu_2p, vectorize.args = c("t", "z"))

#--------------------------- Diverse ---------------------------


predictions_mu2p<-mu2p_vec(xseq,wseq)
predictions_mu2p_dis<-mu_vec(i,xseq,useq)
mu_SDp_2<-predictions_mu2p+predictions_mu2p_dis


integrand<-exp(-trapezoidal_integration_cum(tseq,mu_SDp_2))
trapezoidal_integration(tseq[-1],integrand)


#---------- Plot til at overbevise mig selv om det er sandt -------
# Plot den første graf

#plot(xseq, mu_SDp_1, type = "l", col = "blue", lwd = 2, 
#    xlab = "X-akse", ylab = "Y-akse", 
#    main = "Graf over mu_SDp_2 og mu_SDp_1")

# Tilføj den anden graf
#lines(xseq, mu_SDp_2, col = "red", lwd = 2)

# Tilføj en forklaring (legend)
#legend("topright", legend = c("mu_SDp_1", "mu_SDp_2"), 
#     col = c("blue", "red"), lwd = 2)

#Fordi vi før tog den gennemsnitlige OE rate hele tiden, er middeltiden blevet højre nu, hvor vi indsætter de ægte intensiteter