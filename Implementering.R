

#Function that calculates mean of mu used in Trapez rule
mean_of_mu <- function(row) {
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

#Define grid
t_0<-30
u<-1/12
slut<-31
h<-1/12
N_time<-round((slut-t_0)/h)
N_duration<-round((slut-t_0+u)/h)
ssh<-array(NA,c(N_time+1,N_duration+1,6,6)) #Time, duration, from state, end state

#Boundary conditions
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h),i,j]<-0 ; ssh[1,u/h+1,i,j]<-1
    }
    else{
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h+1),i,j]<-0
      ssh[1:(N_time+1),1,i,j] <- 0 ; ssh[1,1:(u/h+1),i,j]<-0
    }
  }
}

for (i in 1:1){ #change to 1:6 to run for all from states
  i<-1 #i denotes the from state, specify here or remove to run for all from states

for (n in 0:((slut-t_0)/h-1)){
  
  delta<-array(NA,c((u/h+n),6)) #Last entrance denotes the end state
  integrand_neq<-array(NA,c((u/h+n),6))
  intval_neq<-array(NA,c((u/h+1+n),6))
  integrand_pos<-array(NA,c((u/h+1+n),6))
  intval_pos<-array(NA,6)
  
  for (j in 1:6){ #loop through all end states
    delta[,j]<-ssh[n+1,2:(u/h+1+n),i,j]-ssh[n+1,1:(u/h+n),i,j]
    
    integrand_neq[,j]<-delta[,j]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu_p(j,t_0+n*h,z))))
    intval_neq[,j]<-c(0,cumsum(integrand_neq[,j]))
    
    integrand_pos[,j]<-0
    for (k in 1:6){
      if (k!=j){
        integrand_pos[,j]<-integrand_pos[,j]+delta[,k]*mean_of_mu(as.numeric(lapply(seq(0,u+n*h,h),function(z) mu_(k,j,t_0+n*h,z))))
        }
      }
    intval_pos[j]<-sum(integrand_pos[,j])
    for (z in 0:(n+u/h)){ #fill out a row for all to states
      
    }
  }
}
}

