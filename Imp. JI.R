
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
mu_int <- readRDS("mu_array.rds")

mu<-function(i,j,t,u){
  mu_int[(t-t_0)/h+1,u/h+1,i,j]
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
      mu_p_int[,,i]<-mu_p_int[,,i]+mu_int[,,i,j]
    }
  }
}

mu_p<-function(i,t,u){
  mu_p_int[(t-t_0)/h+1,u/h+1,i]
}

#First transition probabilities within J^I is calculated

#Define grid
t_0<-40
u<-1/12
slut<-43
h<-1/12 #has to be the same as h used in "Intensitetsmatricer"
N_time<-round((slut-t_0)/h)
N_duration<-round((slut-t_0+u)/h)
ssh<-array(NA,c(N_time+1,N_duration+1,6,8)) #Time, duration, from state, end state

for (i in 1:6){ #change to 1:6 to run for all "from" states
  i<-1 #i denotes the "from" state, specify here or remove to run for all "from" states
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
    
    delta<-array(NA,c((u/h+n),6)) #Last entrance denotes the end state
    for (j in 1:6){ #loop through all end states
      delta[,j]<-ssh[n+1,2:(u/h+1+n),i,j]-ssh[n+1,1:(u/h+n),i,j] #calculate increments of previous ssh
    }
    for (j in 1:6){ #loop through all end states
      integrand_neq<-delta[,j]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu_p(j,t_0+n*h,z))))
      intval_neq<-c(0,cumsum(integrand_neq))
      
      integrand_pos<-0
      for (k in 1:6){ #positive part consists of sum over the 5 other states
        if (k!=j){
          integrand_pos<-integrand_pos+delta[,k]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu(k,j,t_0+n*h,z))))
        }
      }
      intval_pos<-sum(integrand_pos)
      for (z in 0:(n+u/h)){ #fill out a row for end state j in J^I
        ssh[n+2,z+2,i,j]<-ssh[n+1,z+1,i,j]+h*(-intval_neq[z+1]+intval_pos)
      }
    }
  }
}

#Check that probabilities sum to one
diag<-0
for (j in 1:6){
  diag<-diag+diag(ssh[,(u/h):N_duration+1,1,j])
}
diag

#Plot transition probabilities with maximum duration
for (j in 1:6){
  data <- data.frame(
    time = seq(40,43,1/12),
    p_1j = diag(ssh[,(u/h):N_duration+1,1,j])
  )
  p<-ggplot(data, aes(x = time, y = p_1j)) +
    geom_line() +              # Add a line plot
    #geom_point() +             # Add points to the plot
    labs(x = "Time", y = paste("p_1",j))
  print(p)
  #plot(diag(ssh[,(u/h):N_duration+1,1,j]), type="l",main=j)
}

#Second probabilities in the 3 state model. Disabled, reactivated, dead is calculated



