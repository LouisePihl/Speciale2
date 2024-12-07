# Forbered data
# t_seq: Alder sekvens (18-67)
# u_seq: Varighed sekvens (0-6)


t_seq <- seq(18, 67, by = 1/12)  # Aldersinterval (18-67 år)
u_seq <- seq(0, 49, by = 1/12)    # Varighed (0-6)

# Ekstraher de prædikterede værdier fra mu_int[,,1,2]
mu_values <- mu_int[,,1,6]  # Dette skal være en matrix af dimensioner (length(t_seq), length(u_seq))

# Først, sørg for at have ggplot2 pakken installeret og indlæst
library(ggplot2)

# Forbered data til plottene:
# 1. Plot prædikterede værdier overfor alder (aggregeret over varighed)
avg_mu_age <- apply(mu_values, 1, mean)  # Gennemsnit af mu_values over varighed
df_age <- data.frame(age = t_seq, Value = avg_mu_age, Rate = "predicted_OE")

# 2. Plot prædikterede værdier overfor varighed (aggregeret over alder)
avg_mu_duration <- apply(mu_values, 2, mean)  # Gennemsnit af mu_values over alder
df_duration <- data.frame(duration = u_seq, Value = avg_mu_duration, Rate = "predicted_OE")

# Opret plot for prædikterede værdier overfor alder
plot_OE_age <- ggplot(df_age, aes(x = age, y = Value, color = Rate)) + # Plot prikkerne
  geom_line(aes(linetype = Rate), size = 1.1) +  # Tilføj linje for prædikterede værdier
  scale_color_manual(values = c("predicted_OE" = "steelblue")) +  # Farvevalg
  scale_linetype_manual(values = c("predicted_OE" = "solid")) +  # Linjestil
  scale_shape_manual(values = c("predicted_OE" = 16)) +  # Shape: cirkler
  labs(
    x = "Age",
    y = "OE rate",
    title = ""
  ) +
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern legende for Rate
  )

# Opret plot for prædikterede værdier overfor varighed
plot_OE_duration <- ggplot(df_duration, aes(x = duration, y = Value, color = Rate)) + # Plot prikkerne
  geom_line(aes(linetype = Rate), size = 1.1) +  # Tilføj linje for prædikterede værdier
  scale_color_manual(values = c("predicted_OE" = "steelblue")) +  # Farvevalg
  scale_linetype_manual(values = c("predicted_OE" = "solid")) +  # Linjestil
  scale_shape_manual(values = c("predicted_OE" = 16)) +  # Shape: cirkler
  labs(
    x = "Duration",
    y = "OE rate",
    title = ""
  ) +
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern legende for Rate
  ) + xlim(0,5.5)

# Vis plottet for prædikterede værdier overfor alder og varighed
plot_OE_age
plot_OE_duration

