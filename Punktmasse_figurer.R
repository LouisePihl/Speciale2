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

# Plot using ggplot2
p<-ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1) +            # Line for each segment
  geom_point(data = jump_point1, aes(x = x, y = y),    # Specify data for the jump point
             color = "black", size = 3,shape = 1) + 
  geom_point(data = jump_point2, aes(x = x, y = y),    # Specify data for the jump point
             color = "black", size = 3) +# Red point for jump
  labs(
       x = "Duration",
       y = "Jump rate") +
  ylim(c(0, 0.6)) + 
  theme_minimal()

ggsave("case1.png", p, width = 3, height = 3)

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

# Plot using ggplot2
p<-ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1) +            # Line for each segment
  geom_point(data = jump_point1, aes(x = x, y = y),    # Jump point 1 (hollow circle)
             color = "black", size = 3, shape = 1) +
  geom_point(data = jump_point2, aes(x = x, y = y),    # Jump point 2 (filled circle)
             color = "black", size = 3) +
  labs(
    x = "Duration",
    y = "Jump rate"
  ) +
  ylim(c(0, 0.6)) + 
  theme_minimal()

ggsave("case2.png", p, width = 3, height = 3)

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

# Plot using ggplot2
p<-ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(group = group), size = 1) +            # Line for each segment
  labs(
    x = "Duration",
    y = "Jump rate"
  ) +
  ylim(c(0, 2.1)) +  # Limit the x-axis to [0, 2.1]
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),     
    axis.title.y = element_text(size = 12))+
  theme_minimal()

ggsave("case3.png", p, width = 3, height = 3)