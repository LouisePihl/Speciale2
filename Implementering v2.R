

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

#Define grid
t_0<-30
u<-0
slut<-67
h<-1/12 #has to be the same or larger as h used in "Intensitetsmatricer"
N_time<-round((slut-t_0)/h)
N_duration<-round((slut-t_0+u)/h)
ssh<-array(NA,c(N_time+1,N_duration+1,6,6)) #Time, duration, from state, end state

#Defines mu functions for testing
mu<-function(i,j,t,u){
  0.005
}

mu_p<-function(i,t,u){
  7*0.005
}

start.time <- Sys.time()

for (i in 1:1){ #change to 1:6 to run for all "from" states
  i<-1 #i denotes the "from" state, specify here or remove to run for all "from" states
  #Boundary conditions
  for (j in 1:6){ #loop through all end states
    if (i==j){
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h),i,j]<-0 ; ssh[1,u/h+1,i,j]<-1
    }
    else{
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h+1),i,j]<-0
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
      for (k in 1:6){
        if (k!=j){
          integrand_pos<-integrand_pos+delta[,k]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu(k,j,t_0+n*h,z))))
        }
      }
      intval_pos<-sum(integrand_pos)
      for (z in 0:(n+u/h)){ #fill out a row for end state j
        ssh[n+2,z+2,i,j]<-ssh[n+1,z+1,i,j]+h*(-intval_neq[z+1]+intval_pos)
      }
    }
  }
}

end.time <- Sys.time() #Measures endtime
time.taken <- round(end.time - start.time,2)
time.taken #time it takes
