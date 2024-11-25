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





p<-ggplot(data, aes(x = time, y = p_1j)) +
  geom_line(color = "steelblue", size = 1.1) +               # Add a line plot
  #geom_point() +             # Add points to the plot
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),     
    axis.title.y = element_text(size = 12)
  )+
  labs(x = "Age", y = "Probability")
print(p)
ggsave(paste0("p1", j, "_4067.png"), plot = p, width = 6, height = 3, dpi = 300)
#plot(diag(ssh[,(u/h):N_duration+1,1,j]), type="l",main=j)
}








#-------- Nyt plot med vores farver ------- 
library(ggplot2)
library(dplyr)

# Definer intervaller
z_seq <- seq(0, 10, length.out = 500)  # Duration
t_seq <- seq(20, 67, by = 1/12)        # Age

# Dataframes for plots
df_mu_12_z <- data.frame(
  z = z_seq,
  mu_12 = rep(mu_12(40, z_seq), length(z_seq))
)

df_mu_21_z <- data.frame(
  z = z_seq,
  mu_21 = mu_21(40, z_seq)
)

df_mu_23_z <- data.frame(
  z = z_seq,
  mu_23 = mu_23(40, z_seq),
  mu_13 = rep(mu_13(40), length(z_seq))
)

df_mu_12_t <- data.frame(
  t = t_seq,
  mu_12 = mu_12(t_seq, 1)
)

df_mu_21_t <- data.frame(
  t = t_seq,
  mu_21 = mu_21(t_seq, 1)
)

df_mu_23_t <- data.frame(
  t = t_seq,
  mu_23 = mu_23(t_seq, 1),
  mu_13 = mu_13(t_seq)
)

# Plot for mu_12 over Duration (z)
plot_mu_12_z <- ggplot(df_mu_12_z, aes(x = z, y = mu_12)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(x = "Duration", y = expression(mu[12])) +
  theme_minimal()

# Plot for mu_21 over Duration (z)
plot_mu_21_z <- ggplot(df_mu_21_z, aes(x = z, y = mu_21)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(x = "Duration", y = expression(mu[21])) +
  theme_minimal()

# Plot for mu_23 and mu_13 over Duration (z)
plot_mu_23_z <- ggplot(df_mu_23_z, aes(x = z)) +
  geom_line(aes(y = mu_23), color = "steelblue", size = 1.5) +
  geom_hline(aes(yintercept = mu_13), color = "hotpink2", size = 1.5) +
  labs(x = "Duration", y = "", title = expression(mu[23] ~ "and" ~ mu[13])) +
  theme_minimal() +
  ylim(0, 0.015) +
  theme(legend.position = "topright") +
  annotate("text", x = 8, y = 0.014, label = "mu_23 (age=40)", color = "steelblue") +
  annotate("text", x = 8, y = 0.012, label = "mu_13 (age=40)", color = "hotpink2")

# Plot for mu_12 over Age (t)
plot_mu_12_t <- ggplot(df_mu_12_t, aes(x = t, y = mu_12)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(x = "Age", y = expression(mu[12])) +
  theme_minimal()

# Plot for mu_21 over Age (t)
plot_mu_21_t <- ggplot(df_mu_21_t, aes(x = t, y = mu_21)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(x = "Age", y = expression(mu[21])) +
  theme_minimal()



#---------- Plot for mu_i3 -------
# Plot for mu_23 and mu_13 over Age (t)
  # Plot for mu_23 and mu_13 over Age (t)
  plot_mu_23_t <- ggplot(df_mu_23_t, aes(x = t)) +
    geom_line(aes(y = mu_23), color = "steelblue", size = 1.1) +
    geom_line(aes(y = mu_13), color = "hotpink2", size = 1.1) +
    labs(x = "Age", y = "", title = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),     
      axis.title.y = element_text(size = 12)
    )+
    labs(x = "Age", y = "") +
    annotate("text", x = 58, y = 0.06, 
             label = expression(mu[23] ~ "(duration=1)"), 
             color = "steelblue") +
    annotate("text", x = 60, y = 0.008, 
             label = expression(mu[13] ~ "(duration=1)"), 
             color = "hotpink2")
  ggsave(paste0("p_i3.png"), plot_mu_23_t, width = 6, height = 4, dpi = 300)
    
    
    
    # Evaluér mu_13 for t = 40
    mu_13_value <- mu_13(40)  # Sørg for, at mu_13 kan evaluere til en numerisk værdi
    
    # Skab plottet
    plot_mu_23_z <- ggplot(df_mu_23_z, aes(x = z)) +
      geom_line(aes(y = mu_23), color = "steelblue", size = 1.1) +
      geom_hline(yintercept = mu_13_value, color = "hotpink2", size = 1.1) +  # Brug konstant værdi
      labs(x = "Duration", y = "", title = "") +
      theme_minimal() +
      ylim(0, 0.015) +
      theme(
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),     
        axis.title.y = element_text(size = 12)
      ) +
      annotate("text", x = 8, y = 0.0055, label = expression(mu[23] ~ "(age=40)"), 
               color = "steelblue") +
      annotate("text", x = 8, y = 0.0017, label = expression(mu[13] ~ "(age=40)"), 
               color = "hotpink2")
    
    # Gem plottet som en fil
    ggsave("p_i3_z.png", plot = plot_mu_23_z, width = 6, height = 4, dpi = 300)


# Print plots
print(plot_mu_12_z)
print(plot_mu_21_z)
print(plot_mu_23_z)
print(plot_mu_12_t)
print(plot_mu_21_t)
print(plot_mu_23_t)
