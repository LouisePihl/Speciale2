
mu_12_f <-function(t,z){0.01034251*(t>67)+exp(-22.93861+0.8512794751*t+0.007038620628*t^2-0.001014142721*t^3+1.910555732* 10^(-5)*t^4-1.112835873* 10^(-7)*t^5)*(t<=67)}
mu_12_m <-function(t,z){0.0009687435*(t>67) +(exp(72.53851-10.66927*t+0.53371*t^{2}-0.012798*t^3+1.4922*10^{-4}*t^{4}-6.8007*10^{-7}*t^5))*(t<=67)}

mu_21_f <- function(t,z){exp(0.5640359-0.1035612*t)*(z>5)+exp(0.4279071-0.0314083*t-0.4581508*z)*(5>=z&z>2)+exp(1.517019-0.0314083*t-1.0027067*z)*(2>=z&z>0.2291667)+exp(0.8694878-0.0314083*t+1.8228841*z)*(0.2291667>=z)}
mu_23_f <- function(t,z){exp(-8.2226723+0.0696388*t)*(z>5)+(exp(-7.0243455+0.0685318*t-0.2207053*z))*(z<=5)}

mu_21_m <- function(t,z){exp(-0.042168-0.092455*t)*(z>5)+exp(-0.4808001-0.0309126*t-0.335552*z)*(5>=z&z>2)+exp(0.3766531-0.0309126*t-0.7642786*z)*(2>=z&z>0.2291667)+exp(-0.9148875-0.0309126*t+4.8715347*z)*(0.2291667>=z)}
mu_23_m <- function(t,z){exp(-11.9169277+0.1356766*t)*(z>5)+(exp(-6.1057464+0.0635736*t-0.2891195*z))*(z<=5)}

#t_values <- seq(18, 67, by = 1/12)
#z_values <- rep(1, length(t_values))  

z_values <- seq(0, 10, by = 1/12)
t_values <- rep(40, length(z_values)) 

df <- data.frame(
  t = t_values,
  z = z_values,
  mu_12_f = mapply(mu_12_f, t_values, z_values),
  mu_12_m = mapply(mu_12_m, t_values, z_values),
  mu_21_f = mapply(mu_21_f, t_values, z_values),
  mu_21_m = mapply(mu_21_m, t_values, z_values),
  mu_23_f = mapply(mu_23_f, t_values, z_values),
  mu_23_m = mapply(mu_23_m, t_values, z_values)
)

p <- ggplot() +
  # Base scenarie
  geom_line(data = df, aes(x = z, y = mu_12_f, color = "Female"), size = 1.1, linetype = "solid") +
  # Mortality stress scenarie
  geom_line(data = df, aes(x = z, y = mu_12_m, color = "Male"), size = 1.1, linetype = "solid") +
  # Labels og tema
  labs(x = "Duration", y = expression(mu[DIRA](40,u))) +  # Fjern legend titel
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),  # Flyt legend ind i grafen (x, y koordinater)  # Tilføj baggrund til legenden
                        legend.key = element_blank(),  # Fjern nøgle-rammer
                        legend.title = element_blank(),  # Fjern legend titel
                        axis.text.x = element_text(size = 14), 
                        axis.text.y = element_text(size = 14),
                        axis.title.x = element_text(size = 14),     
                        axis.title.y = element_text(size = 14),
                        axis.text = element_text(size = 14),
                        text = element_text(size = 14, family = "Times New Roman", color = "black"),
                        axis.ticks = element_line(size = 0.15, color = "black"),
                        axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
    ) +
      # Specificer farver for scenarierne
      scale_color_manual(values = c("Female" = "steelblue", "Male" = "aquamarine3")) 
    
    print(p)
    
    # Gem plot
    ggsave("mu_12_mf_u.png", plot = p, width = 6, height = 3, dpi = 300)
    