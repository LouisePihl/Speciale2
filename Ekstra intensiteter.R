mu_21 <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23 <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}
mu_2p<-function(t,z){mu_21(t,z)+mu_23(t,z)}
mu2p_vec <- Vectorize(mu_2p, vectorize.args = c("t", "z"))
mu_12 <-function(t,z){0.01034251*(t>67)+exp(-22.93861+0.8512794751*t+0.007038620628*t^2-0.001014142721*t^3+1.910555732* 10^(-5)*t^4-1.112835873* 10^(-7)*t^5)*(t<=67)}
library("readxl")
levetidbench <- read_excel("Benchmark_doedelighed2022_010224.xlsx")
mu_13 <- function (t){
  levetidbench$Kvinder[t+1]
}

z_seq<-seq(0,6,1/12)

plot(z_seq,rep(mu_12(40,z_seq),length(z_seq)), type="n",xlab="Duration",ylab="mu_12")
lines(z_seq,rep(mu_12(40,z_seq),length(z_seq)),col="blue",lwd=2)

plot(z_seq,mu_21(40,z_seq), type="n",xlab="Duration",ylab="mu_21")
lines(z_seq,mu_21(40,z_seq),col="blue",lwd=2)

plot(z_seq,mu_23(40,z_seq), type="n",xlab="Duration",ylab="",ylim=c(0,0.015))
lines(z_seq,mu_23(40,z_seq),col="blue",lwd=2)
abline(h=mu_13(40),col="green",lwd=2)
legend("topright", legend=c("mu_23 (age=40)", "mu_13 (age=40)"), col=c("blue", "green"), lwd=2,bty = "n")

t_seq<-seq(20,67,1/12)

plot(t_seq,mu_12(t_seq,1), type="n",xlab="Age",ylab="mu_12")
lines(t_seq,mu_12(t_seq,1),col="blue",lwd=2)

plot(t_seq,mu_21(t_seq,1), type="n",xlab="Age",ylab="mu_21")
lines(t_seq,mu_21(t_seq,1),col="blue",lwd=2)

plot(t_seq,mu_23(t_seq,1), type="n",xlab="Age",ylab="")
lines(t_seq,mu_23(t_seq,1),col="blue",lwd=2)
lines(t_seq,mu_13(t_seq),col="green",lwd=2)
legend("topleft", legend=c("mu_23 (duration=1)", "mu_13 (duration=1)"), col=c("blue", "green"), lwd=2,bty = "n")


mu_12 <-function(t,z){0.0009687435*(t>67)+(exp(72.53851-10.66927*t+0.53371*t^{2}-0.012798*t^3+1.4922*10^{-4}*t^{4}-6.8007*10^{-7}*t^5))*(t<=67)}
