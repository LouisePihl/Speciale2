
#Function that calculates mean of mu used in Trapez rule
mean_of_mu <- function(row) {
  if (length(row)==1){ 
    return(row)
  }
  else{
    # Initialize a vector to store the results
    summed_row <- numeric(length(row))
    # First element doesn't have a left neighbor, so it stays the same
    summed_row[1] <- row[1]
    # Sum each element with the element to its left
    for (i in 2:length(row)) {
      summed_row[i] <- (row[i] + row[i - 1])/2
    }
    # Return the last 3 entries of the summed row
    return(tail(summed_row, length(row)-1))
  }
}

#Load array with intensities and define mu and mu_p functions
#setwd("/Users/louisepihl/Documents/Speciale2")
#setwd("/Users/frejalundfredholm/Desktop/Speciale/Speciale2")
mu_int <- readRDS("mu_array.rds")
#mu_int_stress1<-mu_int
#mu_int_stress1[,,1,2]<-mu_int[,,1,2]*1.2 #Increase intensity from SB til JC by 10% for all times and durations
#mu_int_stress1[,,,6]<-mu_int[,,,6]*0.8
#mu_int_stress1[,,2,4]<-mu_int[,,2,4]*1.15
#mu_int_stress1[,,2,5]<-mu_int[,,2,5]*1.15


mu<-function(i,j,t,u){
  mu_int[(t-18)/h+1,u/h+1,i,j] #Change here in stress scenarios
}

#Define new mu_p without death and reactivation
t_0<-18
t_slut<-67
u_0<-0
u_slut<-t_slut-t_0
h<-1/12
N_time<-round((t_slut-t_0)/h)
N_duration<-round((u_slut-u_0)/h)
mu_p_int<-array(0,c(N_time+1,N_duration+1,6)) 
for (i in 1:6){
  #mu_p_int[,,i]<-mu_p_int[,,i]+mu_int[,,i,7]+mu_int[,,i,8]
  for (j in 1:6){
    if (j!=i){
      mu_p_int[,,i]<-mu_p_int[,,i]+mu_int[,,i,j] #Change here in stress scenarios
    }
  }
}

mu_p<-function(i,t,u){
  mu_p_int[(t-18)/h+1,u/h+1,i]
}

#First transition probabilities within J^I is calculated

#Define grid
t_0<-40
u<-0
slut<-67
h<-1/12 #has to be the same as h used in "Intensitetsmatricer"
N_time<-round((slut-t_0)/h)
N_duration<-round((slut-t_0+u)/h)
ssh<-array(NA,c(N_time+1,N_duration+1,6,8)) #Time, duration, from state, end state

Vdis_u<-c(2/12,0.5,0.75,1.5,2,3,4,5)-1/12
Vdis_u<-sort(Vdis_u)
Vdis_t<-c(40,55,60)-1/12
Vdis_t<-sort(Vdis_t-t_0)
Vdis_t<-Vdis_t[Vdis_t>=0]
step_seq<-seq(0,slut-t_0,h)
dis_step_t<-findInterval(Vdis_t,step_seq)
dis_step_u<-findInterval(Vdis_u,step_seq)

mean_of_mu <- function(row,n) {
  if (length(row)==1){ 
    return(row)
  }
  else if (n%in%dis_step_t){
    return(row[-length(row)])
  }
  else{
    # Initialize a vector to store the results
    summed_row <- numeric(length(row))
    # First element doesn't have a left neighbor, so it stays the same
    summed_row[1] <- row[1]
    # Sum each element with the element to its left
    for (i in 2:length(row)) {
      summed_row[i] <- (row[i] + row[i - 1])/2
    }
    summed_row[dis_step_u[dis_step_u<=length(row)]]<-row[dis_step_u[dis_step_u<=length(row)]]
    # Return the last 3 entries of the summed row
    return(tail(summed_row, length(row)-1))
  }
}


#start.time <- Sys.time() #To measure run time
for (i in 1:1){ #change to 1:6 to run for all "from" states
  i<-i #i denotes the "from" state, specify here or remove to run for all "from" states
  #Boundary conditions
  for (j in 1:6){ #loop through all end states
    if (i==j){
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h),i,j]<-0 ; ssh[1,u/h+1,i,j]<-1
    }
    else{
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h+1),i,j]<-0
    }
  }
  
  for (n in 0:((slut-t_0)/h-1)){ #loop over all rows 
    if (u/h+n==0){
      delta<-array(NA,c(1,6))
    }
    else{
    delta<-array(NA,c((u/h+n),6))
    }
     #Last entrance denotes the end state
    for (j in 1:6){ #loop through all end states
      if (u/h+n==0){
        delta[,j]<-ssh[n+1,(u/h+1+n),i,j]
      }
      else{
      delta[,j]<-ssh[n+1,2:(u/h+1+n),i,j]-ssh[n+1,1:(u/h+n),i,j] #calculate increments of previous ssh
      }
    }
    for (j in 1:6){ #loop through all end states
      integrand_neq<-delta[,j]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu_p(j,t_0+n*h,z))),n)
      intval_neq<-c(0,cumsum(integrand_neq))
      if ((n+u/h)==0){
        intval_neq<-cumsum(integrand_neq)
      }
      
      integrand_pos<-0
      for (k in 1:6){ #positive part consists of sum over the 5 other states
        if (k!=j){
          integrand_pos<-integrand_pos+delta[,k]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu(k,j,t_0+n*h,z))),n)
        }
      }
      intval_pos<-sum(integrand_pos)
      for (z in 0:(n+u/h)){ #fill out a row for end state j in J^I
        ssh[n+2,z+2,i,j]<-ssh[n+1,z+1,i,j]+h*(-intval_neq[z+1]+intval_pos)
      }
    }
  }
}
#end.time <- Sys.time() #Measures endtime
#time.taken <- round(end.time - start.time,2)
#time.taken #time it takes
#Check that probabilities sum to one
diag<-0
for (j in 1:6){
  diag<-diag+diag(ssh[,(u/h):N_duration+1,1,j])
}
diag
#
##Plot transition probabilities with maximum duration
#for (j in 1:6){
#  data <- data.frame(
#    time = seq(t_0,slut,h),
#    p_1j = diag(ssh[,(u/h):N_duration+1,1,j])
#  )
#  p<-ggplot(data, aes(x = time, y = p_1j)) +
#    geom_line() +              # Add a line plot
#    #geom_point() +             # Add points to the plot
#    labs(x = "Time", y = paste("p_1",j))
#  print(p)
#  #plot(diag(ssh[,(u/h):N_duration+1,1,j]), type="l",main=j)
#}
#
##Probability of staying in DI is calculated by a simple integral
#
mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}

#mu_23_stress<-function(t,z){mu_23(t,z)*1.15}

integrand <- function(t, z) {
  mu_21(t, z) + mu_23(t, z)
}
#
# Opret en sekvens af t og z værdier med trin h = 1/12 (månedlige trin)
t_values <- seq(t_0, slut, by = h)
z_values <- seq(u, u+slut-t_0, by = h)

# Initialiser en vektor til at gemme resultaterne og den akkumulerede sum
results <- numeric(length(t_values))
accumulated_sum <- 0

# Start første trin ved at evaluere ved første t og z
t_lower <- t_values[1]
z_lower <- z_values[1]

# Loop over de resterende værdier af t og z, og akkumuler kun det nye inkrement
for (i in 2:length(t_values)) {
  t_upper <- t_values[i]
  z_upper <- z_values[i]
  
  # Integrer kun over det lille interval fra forrige til nuværende step
  increment <- integrand(t_upper, z_upper) * h
  
  # Læg inkrementet til den akkumulerede sum
  accumulated_sum <- accumulated_sum + increment
  results[i] <- accumulated_sum
}

#exp(-results)

#setwd("/Users/louisepihl/Documents/Speciale")
for (j in 1:6){
  data <- data.frame(
    time = seq(t_0,slut,h),
    p_1j = diag(ssh[,(u/h):N_duration+1,1,j])*exp(-results)
    #p_1j = diag(ssh[1:(12*3+1),1:(12*5+1),1,j])*exp(-results)[1:(12*3+1)]
  )
  p<-ggplot(data, aes(x = time, y = p_1j)) +
    geom_line() +               # Add a line plot
    #geom_point() +             # Add points to the plot
    theme(
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),     
      axis.title.y = element_text(size = 12)
    )+
    labs(x = "Age", y = "Probability")
  print(p)
  #ggsave(paste0("p1", j, "_4067.png"), plot = p, width = 6, height = 3, dpi = 300)
  #plot(diag(ssh[,(u/h):N_duration+1,1,j]), type="l",main=j)
}

#Summen af de 6 ssh'er skal give ssh for at forblive syg
diag<-0
for (j in 1:6){
  diag<-diag+diag(ssh[,(u/h):N_duration+1,1,j])*exp(-results[1:(N_duration+1-(u/h))])
}
diag==exp(-results)
deviation <- abs(diag - exp(-results))
deviation<10^(-10) #stemmer på min. 10'ende decimal

data <- data.frame(
  time = seq(t_0,slut,h),
  p_DIDI = exp(-results)
)
p<-ggplot(data, aes(x = time, y = p_DIDI)) +
  geom_line() +               # Add a line plot
  #geom_point() +             # Add points to the plot
  labs(x = "Time", y = paste("p_DIDI"))
print(p)
