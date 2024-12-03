#------------Layout plots------------


#Punktmasse



x1 <- seq(0, 2, length.out = 100) # First segment
y1 <- 0.1 + (0.2 - 0.1) * (1 - exp(-2 * (x1 - min(x1)) / (max(x1) - min(x1))))

x2 <- seq(2, 6, length.out = 100) # Second segment (after the jump)
y2 <- 0.1 + (0.5 - 0.1) * exp(-1 * (x2 - min(x2)) / (max(x2) - min(x2)))

# Combine into a data frame
df <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  group = rep(c("Segment 1", "Segment 2"), each = 100)
)

# Define the jump point
jump_point1 <- data.frame(x = 2, y = 0.1 + (0.2 - 0.1) * (1 - exp(-2 * (2 - min(x1)) / (max(x1) - min(x1)))))
jump_point2 <- data.frame(x = 2, y = 0.1 + (0.5 - 0.1) * exp(-1 * (2 - min(x2)) / (max(x2) - min(x2))))




p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1, color = "black") +  # Linjer for segmenter
  geom_point(data = jump_point1, aes(x = x, y = y), color = "black", size = 3, shape = 1) +
  geom_point(data = jump_point2, aes(x = x, y = y), color = "black", size = 3) +
  labs(x = "Duration", y = "Jump rate") +
  ylim(c(0, 0.6)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern forklaring
  )

ggsave("case1.png", p1, width = 6, height = 4, dpi = 300)



x1 <- seq(0, 2, length.out = 100)
y1 <- 0.1 + (0.2 - 0.1) * (1 - exp(-2 * (x1 - min(x1)) / (max(x1) - min(x1))))

# Second segment: Exponential decay, starting from 0.25
x2 <- seq(2, 6, length.out = 100)
y2 <- 0.1 + (0.25 - 0.1) * exp(-1 * (x2 - min(x2)) / (max(x2) - min(x2)))

# Combine into a data frame
df <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  group = rep(c("Segment 1", "Segment 2"), each = 100)
)

# Define the jump points
jump_point1 <- data.frame(x = 2, y = max(y1))  # Value at the end of the first segment
jump_point2 <- data.frame(x = 2, y = max(y2))  # Starting value of the second segment


p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1, color = "black") +  # Linjer for segmenter
  geom_point(data = jump_point1, aes(x = x, y = y), color = "black", size = 3, shape = 1) +
  geom_point(data = jump_point2, aes(x = x, y = y), color = "black", size = 3) +
  labs(x = "Duration", y = "Jump rate") +
  ylim(c(0, 0.6)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern forklaring
  )

ggsave("case2.png", p2, width = 6, height = 4, dpi = 300)

x1 <- seq(0, 1.9, length.out = 100)
y1 <- 0.1 + (0.2 - 0.1) * (1 - exp(-2 * (x1 - min(x1)) / (max(x1) - min(x1))))

# Second segment: Exponential decay starting from 0.25 (after x = 2.1)
x2 <- seq(2.1, 6, length.out = 100)
y2 <- 0.1 + (0.25 - 0.1) * exp(-1 * (x2 - min(x2)) / (max(x2) - min(x2)))

# Linear segments: From x = 1.9 to x = 2 (rising to 2)
x3 <- seq(1.9, 2, length.out = 10)
y3 <- seq(max(y1), 2, length.out = length(x3))

# Linear segment: From x = 2 to x = 2.1 (falling to where y2 starts)
x4 <- seq(2, 2.1, length.out = 10)
y4 <- seq(2, max(y2), length.out = length(x4))

# Combine all into a data frame
df <- data.frame(
  x = c(x1, x2, x3, x4),
  y = c(y1, y2, y3, y4),
  group = rep(c("Segment 1", "Segment 2", "Linear 1", "Linear 2"), 
              times = c(length(x1), length(x2), length(x3), length(x4)))
)


p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1, color = "black") +  # Linjer for segmenter
  labs(x = "Duration", y = "Jump rate") +
  ylim(c(0, 2.1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern forklaring
  )

ggsave("case3.png", p3, width = 6, height = 4, dpi = 300)




#predplots
# Generer den forudsigte værdi for OE (fra den valgte model)
model <- glm_splines_1
Data$predicted_O <- predict(model, type = "response")

# Tjek efter trends
DurAgg <- Data %>%
  group_by(duration) %>%
  summarise(
    expoAgg = sum(E),
    occAgg = sum(O),
    predictedAgg = sum(predicted_O)
  ) %>%
  mutate(
    predicted_OE = predictedAgg / expoAgg,
    OE = occAgg / expoAgg
  )

# Opret dataramme til ggplot
df <- DurAgg %>%
  pivot_longer(c(OE, predicted_OE), names_to = "Rate", values_to = "Value")

# Lav plottet
plot_OE <- ggplot(df, aes(x = duration, y = Value, color = Rate)) +
  geom_point(aes(shape = Rate), size = 3) +  # Plot prikkerne (fjern trekanter)
  geom_line(aes(linetype = Rate), size = 1.1) +  # Tilføj linjer for de forudsigte værdier
  scale_color_manual(values = c("OE" = "black", "predicted_OE" = "steelblue")) +  # Farvevalg
  scale_linetype_manual(values = c("OE" = "blank", "predicted_OE" = "solid")) +  # Linjestil
  scale_shape_manual(values = c("OE" = 16)) +  # Sæt shape til cirkler (16 er cirklen)
  labs(
    x = "Duration",
    y = "O/E rate",
    title = ""
  ) +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern legende for "Rate" variabelen
  ) 

# Print plottet
print(plot_OE)



# Tjek efter trends
AgeAgg <- Data %>%
  group_by(age) %>%
  summarise(
    expoAgg = sum(E),
    occAgg = sum(O),
    predictedAgg = sum(predicted_O)
  ) %>%
  mutate(
    predicted_OE = predictedAgg / expoAgg,
    OE = occAgg / expoAgg
  )

# Opret dataramme til ggplot
df_age <- AgeAgg %>%
  pivot_longer(c(OE, predicted_OE), names_to = "Rate", values_to = "Value")

# Lav plottet
plot_OE_age <- ggplot(df_age, aes(x = age, y = Value, color = Rate)) +
  geom_point(aes(shape = Rate), size = 3) +  # Plot prikkerne (fjern trekanter)
  geom_line(aes(linetype = Rate), size = 1.1) +  # Tilføj linjer for de forudsigte værdier
  scale_color_manual(values = c("OE" = "black", "predicted_OE" = "steelblue")) +  # Farvevalg
  scale_linetype_manual(values = c("OE" = "blank", "predicted_OE" = "solid")) +  # Linjestil
  scale_shape_manual(values = c("OE" = 16)) +  # Sæt shape til cirkler (16 er cirklen)
  labs(
    x = "Age",
    y = "O/E rate",
    title = ""
  ) +  # Etiketter
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = "none"  # Fjern legende for "Rate" variabelen
  )

# Print plottet
print(plot_OE_age)


# Gem plottet som en billedfil
ggsave("RCUB_spline1_dur.png", plot = plot_OE, width = 6, height = 4, dpi = 300)


# Gem plottet som en billedfil
ggsave("RCUB_spline1_age.png", plot = plot_OE_age, width = 6, height = 4, dpi = 300)



# Residual plot for Age
residplotage <- function(model) {
  ggplot(model, aes(x = Data$age, y = .stdresid)) +
    geom_point(size = 1.75) +
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    labs(
      x = "Age",
      y = "Fitted values",
      title = ""
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, family = "Times New Roman"),
      axis.text.y = element_text(size = 14, family = "Times New Roman"),
      axis.title.x = element_text(size = 14, family = "Times New Roman"),
      axis.title.y = element_text(size = 14, family = "Times New Roman"),
      text = element_text(size = 14, family = "Times New Roman", color = "black"),
      axis.ticks = element_line(size = 0.15, color = "black"),
      axis.ticks.length = unit(-0.15, "cm")
    )
}



# Residual plot for Duration
residplotduration <- function(model) {
  ggplot(model, aes(x = Data$duration, y = .stdresid)) +
    geom_point(size = 1.75) +
    geom_smooth(method = "loess", size = 1, formula = y ~ x) +
    labs(
      x = "Duration",
      y = "Fitted values",
      title = ""
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, family = "Times New Roman"),
      axis.text.y = element_text(size = 14, family = "Times New Roman"),
      axis.title.x = element_text(size = 14, family = "Times New Roman"),
      axis.title.y = element_text(size = 14, family = "Times New Roman"),
      text = element_text(size = 14, family = "Times New Roman", color = "black"),
      axis.ticks = element_line(size = 0.15, color = "black"),
      axis.ticks.length = unit(-0.15, "cm")
    )
}

res_dur <- residplotduration(model)
res_age <- residplotage(model)


# Gem plottet som en billedfil
ggsave("Res_RCUB_spline1_dur.png", plot = res_dur, width = 6, height = 4, dpi = 300)




#OE 
#-------------------- Nye plots --------------------
library(ggplot2)

plot_age_oe <- ggplot(AgeAgg, aes(x = age, y = OE)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Age", y = "O/E rate") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )
ggsave("=E_JCDP_DUR.png", plot = plot_age_oe, width = 6, height = 4, dpi = 300)


# Print plot
print(plot_age_oe)

plot_duration_oe <- ggplot(DurAgg, aes(x = duration, y = OE)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Duration", y = "O/E rate") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )

# Print plot
print(plot_duration_oe)
ggsave("OE_JCDP_DUR.png", plot = plot_duration_oe, width = 6, height = 4, dpi = 300)


plot_age_occ <- ggplot(AgeAgg, aes(x = age, y = occAgg)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Age", y = "Occurrence") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )

# Print plot
print(plot_age_occ)

plot_duration_occ <- ggplot(DurAgg, aes(x = duration, y = occAgg)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Duration", y = "Occurrence") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )

# Print plot
print(plot_duration_occ)

plot_age_expo <- ggplot(AgeAgg, aes(x = age, y = expoAgg)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Age", y = "Exposure") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )

# Print plot
print(plot_age_expo)
ggsave("E_JCDP_AGE.png", plot = plot_age_expo, width = 6, height = 4, dpi = 300)


plot_duration_expo <- ggplot(DurAgg, aes(x = duration, y = expoAgg)) +
  geom_point(color = "black", size = 3) +  # Plot prikkerne
  labs(x = "Duration", y = "Exposure") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")
  )

ggsave("E_JCDP_DUR.png", plot = plot_duration_expo, width = 6, height = 4, dpi = 300)

# Print plot
print(plot_duration_expo)





plot_combined <- ggplot(CombinedDur, aes(x = duration, y = OE, color = Model)) +
  geom_point(size = 3, aes(shape = Model)) +  # Observerede OE-rater som punkter
  geom_line(aes(y = predicted_OE), size = 1) +  # Forudsagte OE-rater som linjer
  labs(
    x = "Duration",
    y = "O/E Rate",
    color = "",  # Titel for farve forklaring fjernes
    shape = ""   # Titel for shape forklaring fjernes
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position = c(0.75, 0.95),  # Placering af legend
    legend.justification = c(0, 1),  # Justering så den flugter øverst venstre
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key = element_blank(),  # Fjern baggrundsfarve i legend keys
    legend.title = element_text(size = 12, family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman")
  ) +
  scale_color_manual(
    values = c("μ" = "steelblue", "New μ" = "aquamarine3"),
    labels = c(
      "μ" = expression(mu[SBJC]), 
      "New μ" = expression(mu[SBJC]^new)  # Bruger matematiske symboler for toppen
    )
  ) +
  scale_shape_manual(
    values = c("μ" = 16, "New μ" = 16),  # Ens punkttyper
    labels = c(
      "μ" = expression(mu[SBJC]), 
      "New μ" = expression(mu[SBJC]^new)  # Samme labels som for farver
    )
  )

plot(plot_combined)

# Gem plot som fil og vis det
ggsave("OE_rate_plot_with_mu.png", plot_combined, width = 6, height = 4, dpi = 300)