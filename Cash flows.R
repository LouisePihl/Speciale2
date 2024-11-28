
Benefits <- data.frame(Public_1= numeric(6),Insurance_1= numeric(6),Public_2= numeric(6),Insurance_2= numeric(6))

Benefits$Public_1<-c(28780,12330,12330,22330,36830,29540)
Benefits$Insurance_1<-rep(13220,6)
Benefits$Public_2<-c(18780,0,0,12330,26830,18490)
Benefits$Insurance_2<-c(13220,32000,32000,19670,5170,13510)


#Benefits$Public_1<-c(28780,12330,12330,22330,36830,29540)
#Benefits$Insurance_1<-rep(13220,6)
#Benefits$Public_2<-c(18780,0,0,12330,26830,18490)
#Benefits$Insurance_2<-c(13220,32000,32000,19670,5170,13510)
#Benefits$Insurance_2<-c(14159,32000,32000,20287,6164,14966)
#Benefits$Insurance_2<-c(13220,32000,32000,19670,13668,13510)


#Starting in SB
expec_cash_flow_Insurance_1<-data.frame(SB=numeric(N_duration+1-u/h))
expec_cash_flow_Insurance_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_1[1]*exp(-results)
expec_cash_flow_Insurance_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_1[2]*exp(-results)
expec_cash_flow_Insurance_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_1[3]*exp(-results)
expec_cash_flow_Insurance_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_1[4]*exp(-results)
expec_cash_flow_Insurance_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_1[5]*exp(-results)
expec_cash_flow_Insurance_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_1[6]*exp(-results)
expec_cash_flow_Insurance_1$total<-expec_cash_flow_Insurance_1$SB+expec_cash_flow_Insurance_1$JC+expec_cash_flow_Insurance_1$RS+expec_cash_flow_Insurance_1$UB+expec_cash_flow_Insurance_1$FJ+expec_cash_flow_Insurance_1$DP

expec_cash_flow_Public_1<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_1$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_1[1]*exp(-results)
expec_cash_flow_Public_1$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_1[2]*exp(-results)
expec_cash_flow_Public_1$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_1[3]*exp(-results)
expec_cash_flow_Public_1$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_1[4]*exp(-results)
expec_cash_flow_Public_1$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_1[5]*exp(-results)
expec_cash_flow_Public_1$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_1[6]*exp(-results)
expec_cash_flow_Public_1$total<-expec_cash_flow_Public_1$SB+expec_cash_flow_Public_1$JC+expec_cash_flow_Public_1$RS+expec_cash_flow_Public_1$UB+expec_cash_flow_Public_1$FJ+expec_cash_flow_Public_1$DP

expec_cash_flow_Insurance_2<-data.frame(SB=numeric(N_duration+1-u/h))
expec_cash_flow_Insurance_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Insurance_2[1]*exp(-results)
expec_cash_flow_Insurance_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Insurance_2[2]*exp(-results)
expec_cash_flow_Insurance_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Insurance_2[3]*exp(-results)
expec_cash_flow_Insurance_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Insurance_2[4]*exp(-results)
expec_cash_flow_Insurance_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Insurance_2[5]*exp(-results)
expec_cash_flow_Insurance_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Insurance_2[6]*exp(-results)
expec_cash_flow_Insurance_2$total<-expec_cash_flow_Insurance_2$SB+expec_cash_flow_Insurance_2$JC+expec_cash_flow_Insurance_2$RS+expec_cash_flow_Insurance_2$UB+expec_cash_flow_Insurance_2$FJ+expec_cash_flow_Insurance_2$DP

sum(expec_cash_flow_Insurance_2$total)*h


expec_cash_flow_Public_2<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_2$SB<-diag(ssh[,(u/h):N_duration+1,1,1])*Benefits$Public_2[1]*exp(-results)
expec_cash_flow_Public_2$JC<-diag(ssh[,(u/h):N_duration+1,1,2])*Benefits$Public_2[2]*exp(-results)
expec_cash_flow_Public_2$RS<-diag(ssh[,(u/h):N_duration+1,1,3])*Benefits$Public_2[3]*exp(-results)
expec_cash_flow_Public_2$UB<-diag(ssh[,(u/h):N_duration+1,1,4])*Benefits$Public_2[4]*exp(-results)
expec_cash_flow_Public_2$FJ<-diag(ssh[,(u/h):N_duration+1,1,5])*Benefits$Public_2[5]*exp(-results)
expec_cash_flow_Public_2$DP<-diag(ssh[,(u/h):N_duration+1,1,6])*Benefits$Public_2[6]*exp(-results)
expec_cash_flow_Public_2$total<-expec_cash_flow_Public_2$SB+expec_cash_flow_Public_2$JC+expec_cash_flow_Public_2$RS+expec_cash_flow_Public_2$UB+expec_cash_flow_Public_2$FJ+expec_cash_flow_Public_2$DP

#Plots 
data <- data.frame(
  time = seq(40,67,1/12),
  cf_Insurance1 = expec_cash_flow_Insurance_1$total[1:(3*12+1)],
  cf_Insurance2 = expec_cash_flow_Insurance_2$total[1:(3*12+1)]
)
  ggplot(data, aes(x = time, y = cf_Insurance1)) +
  geom_line() +              # Add a line plot
  labs(x = "Time", y = "Expected cash flow")
  
  ggplot(data, aes(x = time, y = cf_Insurance2)) +
    geom_line() +              # Add a line plot
    labs(x = "Time", y = "Expected cash flow")

  
y_min <-12000
y_max <- max(c(data$cf_Insurance1, data$cf_Insurance2))
  
  
  # Plots
  data <- data.frame(
    time = seq(40, 67, 1/12),
    cf_Insurance1 = expec_cash_flow_Insurance_1$total,
    cf_Insurance2 = expec_cash_flow_Insurance_2$total
  )
  
  # Plot for cf_Insurance1
  p1 <- ggplot(data, aes(x = time, y = cf_Insurance1)) +
    geom_line(color = "blue", size = 1.1) +  # Tilføj linjeplot
    labs(x = "Age", y = "Expected cash flow") +  # Etiketter
    theme_bw() +  # Minimalistisk tema
    theme(
      axis.text.x = element_text(size = 14), 
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),     
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 14),
      text = element_text(size = 14, family = "Times New Roman", color = "black"),
      axis.ticks = element_line(size = 0.15, color = "black"),
      axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
    ) +
    ylim(y_min, y_max)
  print(p1)
  ggsave("cf_Insurance1_JI_4067.png", plot = p1, width = 6, height = 3, dpi = 300)
  
  # Plot for cf_Insurance2
  p2 <- ggplot(data, aes(x = time, y = cf_Insurance2)) +
    geom_line(color = "blue", size = 1.1) +  # Tilføj linjeplot
    labs(x = "Age", y = "Expected cash flow") +  # Etiketter
    theme_bw() +  # Minimalistisk tema
    theme(
      axis.text.x = element_text(size = 14), 
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),     
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 14),
      text = element_text(size = 14, family = "Times New Roman", color = "black"),
      axis.ticks = element_line(size = 0.15, color = "black"),
      axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
    ) +
    ylim(y_min, y_max)
  print(p2)
  ggsave("cf_Insurance2_JI_4067.png", plot = p2, width = 6, height = 3, dpi = 300)
  

#Starting in DP
expec_cash_flow_Insurance_1<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Insurance_1$SB<-diag(ssh[,(u/h):N_duration+1,6,1])*Benefits$Insurance_1[1]*exp(-results)
expec_cash_flow_Insurance_1$JC<-diag(ssh[,(u/h):N_duration+1,6,2])*Benefits$Insurance_1[2]*exp(-results)
expec_cash_flow_Insurance_1$RS<-diag(ssh[,(u/h):N_duration+1,6,3])*Benefits$Insurance_1[3]*exp(-results)
expec_cash_flow_Insurance_1$UB<-diag(ssh[,(u/h):N_duration+1,6,4])*Benefits$Insurance_1[4]*exp(-results)
expec_cash_flow_Insurance_1$FJ<-diag(ssh[,(u/h):N_duration+1,6,5])*Benefits$Insurance_1[5]*exp(-results)
expec_cash_flow_Insurance_1$DP<-diag(ssh[,(u/h):N_duration+1,6,6])*Benefits$Insurance_1[6]*exp(-results)
expec_cash_flow_Insurance_1$total<-expec_cash_flow_Insurance_1$SB+expec_cash_flow_Insurance_1$JC+expec_cash_flow_Insurance_1$RS+expec_cash_flow_Insurance_1$UB+expec_cash_flow_Insurance_1$FJ+expec_cash_flow_Insurance_1$DP

expec_cash_flow_Public_1<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_1$SB<-diag(ssh[,(u/h):N_duration+1,6,1])*Benefits$Public_1[1]*exp(-results)
expec_cash_flow_Public_1$JC<-diag(ssh[,(u/h):N_duration+1,6,2])*Benefits$Public_1[2]*exp(-results)
expec_cash_flow_Public_1$RS<-diag(ssh[,(u/h):N_duration+1,6,3])*Benefits$Public_1[3]*exp(-results)
expec_cash_flow_Public_1$UB<-diag(ssh[,(u/h):N_duration+1,6,4])*Benefits$Public_1[4]*exp(-results)
expec_cash_flow_Public_1$FJ<-diag(ssh[,(u/h):N_duration+1,6,5])*Benefits$Public_1[5]*exp(-results)
expec_cash_flow_Public_1$DP<-diag(ssh[,(u/h):N_duration+1,6,6])*Benefits$Public_1[6]*exp(-results)
expec_cash_flow_Public_1$total<-expec_cash_flow_Public_1$SB+expec_cash_flow_Public_1$JC+expec_cash_flow_Public_1$RS+expec_cash_flow_Public_1$UB+expec_cash_flow_Public_1$FJ+expec_cash_flow_Public_1$DP

expec_cash_flow_Insurance_2<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Insurance_2$SB<-diag(ssh[,(u/h):N_duration+1,6,1])*Benefits$Insurance_2[1]*exp(-results)
expec_cash_flow_Insurance_2$JC<-diag(ssh[,(u/h):N_duration+1,6,2])*Benefits$Insurance_2[2]*exp(-results)
expec_cash_flow_Insurance_2$RS<-diag(ssh[,(u/h):N_duration+1,6,3])*Benefits$Insurance_2[3]*exp(-results)
expec_cash_flow_Insurance_2$UB<-diag(ssh[,(u/h):N_duration+1,6,4])*Benefits$Insurance_2[4]*exp(-results)
expec_cash_flow_Insurance_2$FJ<-diag(ssh[,(u/h):N_duration+1,6,5])*Benefits$Insurance_2[5]*exp(-results)
expec_cash_flow_Insurance_2$DP<-diag(ssh[,(u/h):N_duration+1,6,6])*Benefits$Insurance_2[6]*exp(-results)
expec_cash_flow_Insurance_2$total<-expec_cash_flow_Insurance_2$SB+expec_cash_flow_Insurance_2$JC+expec_cash_flow_Insurance_2$RS+expec_cash_flow_Insurance_2$UB+expec_cash_flow_Insurance_2$FJ+expec_cash_flow_Insurance_2$DP

expec_cash_flow_Public_2<-data.frame(SB=numeric(N_duration+1))
expec_cash_flow_Public_2$SB<-diag(ssh[,(u/h):N_duration+1,6,1])*Benefits$Public_2[1]*exp(-results)
expec_cash_flow_Public_2$JC<-diag(ssh[,(u/h):N_duration+1,6,2])*Benefits$Public_2[2]*exp(-results)
expec_cash_flow_Public_2$RS<-diag(ssh[,(u/h):N_duration+1,6,3])*Benefits$Public_2[3]*exp(-results)
expec_cash_flow_Public_2$UB<-diag(ssh[,(u/h):N_duration+1,6,4])*Benefits$Public_2[4]*exp(-results)
expec_cash_flow_Public_2$FJ<-diag(ssh[,(u/h):N_duration+1,6,5])*Benefits$Public_2[5]*exp(-results)
expec_cash_flow_Public_2$DP<-diag(ssh[,(u/h):N_duration+1,6,6])*Benefits$Public_2[6]*exp(-results)
expec_cash_flow_Public_2$total<-expec_cash_flow_Public_2$SB+expec_cash_flow_Public_2$JC+expec_cash_flow_Public_2$RS+expec_cash_flow_Public_2$UB+expec_cash_flow_Public_2$FJ+expec_cash_flow_Public_2$DP

#Plots 
data <- data.frame(
  time = seq(40,43,1/12),
  cf_Insurance1 = expec_cash_flow_Insurance_1$total[1:(3*12+1)],
  cf_Insurance2 = expec_cash_flow_Insurance_2$total[1:(3*12+1)]
)
p1<-ggplot(data, aes(x = time, y = cf_Insurance1)) +
  geom_line() +              # Add a line plot
  labs(x = "Age", y = "Expected cash flow")+
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),     
    axis.title.y = element_text(size = 12)
  )
print(p1)
ggsave("cf_Ins1_4043.png", plot = p1, width = 6, height = 3, dpi = 300)

p2<-ggplot(data, aes(x = time, y = cf_Insurance2)) +
  geom_line() +              # Add a line plot
  labs(x = "Age", y = "Expected cash flow")+
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),     
    axis.title.y = element_text(size = 12)
  )
print(p2)
ggsave("cf_Ins2_4043.png", plot = p2, width = 6, height = 3, dpi = 300)  




# Prepare data
data <- data.frame(
  time = seq(40, 67, 1/12),
  cf_Insurance1 = expec_cash_flow_Insurance_1$total[1:(27*12+1)],
  cf_Insurance2 = expec_cash_flow_Insurance_2$total[1:(27*12+1)]
)

# Plot for cf_Insurance1
p1 <- ggplot(data, aes(x = time, y = cf_Insurance1)) +
  geom_line(color = "steelblue", size = 1.1) +  # Line plot with steelblue color and line width
  theme_bw() +  # Clean white background
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman", color = "black"), 
    axis.text.y = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.title.x = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.title.y = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),  # Custom ticks
    axis.ticks.length = unit(-0.15, "cm"),  # Negative ticks for indentation
    text = element_text(size = 14, family = "Times New Roman", color = "black")  # General text styling
  ) +
  labs(x = "Age", y = "Expected cash flow")  # Axis labels

# Print and save the plot for cf_Insurance1
print(p1)
ggsave("cf_Ins1_4043.png", plot = p1, width = 6, height = 3, dpi = 300)

# Plot for cf_Insurance2
p2 <- ggplot(data, aes(x = time, y = cf_Insurance2)) +
  geom_line(color = "steelblue", size = 1.1) +  # Line plot with steelblue color and line width
  theme_bw() +  # Clean white background
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman", color = "black"), 
    axis.text.y = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.title.x = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.title.y = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),  # Custom ticks
    axis.ticks.length = unit(-0.15, "cm"),  # Negative ticks for indentation
    text = element_text(size = 14, family = "Times New Roman", color = "black")  # General text styling
  ) +
  labs(x = "Age", y = "Expected cash flow")  # Axis labels

# Print and save the plot for cf_Insurance2
print(p2)
ggsave("cf_Ins2_4043.png", plot = p2, width = 6, height = 3, dpi = 300)


