
#Defines array for all intensities
t_0<-18
t_slut<-67
u_0<-0
u_slut<-t_slut-t_0
h<-1/12
N_time<-round((t_slut-t_0)/h)
N_duration<-round((u_slut-u_0)/h)
mu_int<-array(NA,c(N_time+1,N_duration+1,6,8)) 

saveRDS(mu_int, file = "mu_array.rds")
mu_int <- readRDS("mu_array.rds")

mu<-function(i,j,t,u){
  mu_int[t/h+1,u/h+1,i,j]
}

#filling in matrices for disability mortality and reactivation 
mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}

#Define time and duration sequences 
t_seq<-seq(t_0,t_slut,h)
u_seq<-seq(u_0,u_slut,h)

#Define grid
grid <- expand.grid(t = t_seq, z = u_seq)

# Evaluate the functions over the grid, and place in mu_int array
mu_int[,,1,8] <- matrix(apply(grid, 1, function(row) mu_23(row["t"], row["z"])), nrow = length(t_seq), ncol = length(u_seq))
mu_int[,,1,7] <- matrix(apply(grid, 1, function(row) mu_21(row["t"], row["z"])), nrow = length(t_seq), ncol = length(u_seq))

#As disability mortality and reactivation are the same in all disability state, we replicate the matrices for the remaining 5 states
for (j in 2:6){
  mu_int[,,j,8]<-mu_int[,,1,8]
  mu_int[,,j,7]<-mu_int[,,1,7]
}

#Tester udfyldning for SBJA, svarende til 1 til 2 (første del er kopieret fra filen endelige modeller)

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

grid <- expand.grid(age = t_seq, duration = u_seq)
new_data <- data.frame(
  age = grid$age,
  duration = grid$duration,
  E=1
)

#mu_p array and sum that sums all intensities out of a state
predictions <- predict(SDJA_final, newdata = new_data, type = "response")
mu_int[,,1,2]<-matrix(predictions, nrow = length(t_seq), ncol = length(u_seq))

mu_p_int<-array(NA,c(N_time+1,N_duration+1,6)) 
for (i in 1:6){
  mu_p_int[,,i]<-mu_p_int[,,i]+mu_int[,,i,7]+mu_int[,,i,8]
  for (j in 1:6){
    if (j!=i){
      mu_p_int[,,i]<-mu_p_int[,,i]+mu_int[,,i,j]
    }
  }
}
saveRDS(mu_p_int, file = "mu_p_array.rds")
mu_p_int <- readRDS("mu_p_array.rds")

mu_p<-function(i,t,u){
  mu_p_int[i,t/h+1,u/h+1]
}
