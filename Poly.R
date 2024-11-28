#Genskaber figur 2.6 i reg bogen
x_seq<-seq(11,21,0.1)
plot(x_seq,poly(x_seq,5)[,1],type="n",ylim=c(-0.2,0.2))
lines(x_seq,poly(x_seq,5)[,1],col="blue")
lines(x_seq,poly(x_seq,5)[,2],col="red")
lines(x_seq,poly(x_seq,5)[,3],col="green")
lines(x_seq,poly(x_seq,5)[,4],col="purple")
lines(x_seq,poly(x_seq,5)[,5],col="orange")

#Poly basis i age dimensionen
t_seq<-seq(18,67,0.1)
plot(t_seq,poly(t_seq,5)[,1],type="n",ylim=c(-0.15,0.15))
lines(t_seq,poly(t_seq,5)[,1],col="blue")
lines(t_seq,poly(t_seq,5)[,2],col="red")
lines(t_seq,poly(t_seq,5)[,3],col="green")
lines(t_seq,poly(t_seq,5)[,4],col="purple")
lines(t_seq,poly(t_seq,5)[,5],col="orange")

#Poly basis i duration dimensionen
u_seq<-seq(0,3,0.01)
plot(u_seq,poly(u_seq,5)[,1],type="n",ylim=c(-0.2,0.2))
lines(u_seq,poly(u_seq,5)[,1],col="blue")
lines(u_seq,poly(u_seq,5)[,2],col="red")
lines(u_seq,poly(u_seq,5)[,3],col="green")
lines(u_seq,poly(u_seq,5)[,4],col="purple")
lines(u_seq,poly(u_seq,5)[,5],col="orange")




library(ggplot2)
library(tidyr)

# Data: Generer u_seq og polynomier
u_seq <- seq(0, 3, 0.01)
polynomials <- as.data.frame(poly(u_seq, 5))  # Beregn polynomier
colnames(polynomials) <- paste0("Poly", 1:5)  # Navngiv kolonner

# Opret data frame til ggplot
df <- cbind(data.frame(u_seq), polynomials) %>%
  pivot_longer(-u_seq, names_to = "Polynomial", values_to = "Value")

# Farvepaletten "fall"
fall_colors <- c("yellowgreen","blue", "steelblue", "forestgreen", "darkseagreen3")

# Lav plottet
plot_polynomials <- ggplot(df, aes(x = u_seq, y = Value, color = Polynomial)) +
  geom_line(size = 1) +  # Plot linjerne
  scale_color_manual(values = fall_colors) +  # Brug farvepaletten
  labs(x = "", y = "", title = "") +  # Tilføj aksetitler
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    axis.text = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  )


# Print plottet
print(plot_polynomials)

# Gem plottet som en billedfil (hvis nødvendigt)
ggsave("polybasis.png", plot = plot_polynomials, width = 6, height = 4, dpi = 300)


# Data: Generer u_seq og polynomier
u_seq <- seq(0, 3, 0.01)
polynomials <- as.data.frame(poly(u_seq, 5))  # Beregn polynomier
colnames(polynomials) <- paste0("Poly", 1:5)  # Navngiv kolonner

# Opret data frame til ggplot
df <- cbind(data.frame(u_seq), polynomials) %>%
  pivot_longer(-u_seq, names_to = "Polynomial", values_to = "Value")

# Lav plottet
plot_polynomials <- ggplot(df, aes(x = u_seq, y = Value, linetype = Polynomial)) +
  geom_line(color = "black", size = 1) +  # Brug sorte linjer
  labs(x = "", y = "", title = "") +  # Tilføj aksetitler
  theme_bw() +  # Ensartet baggrund
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),     
    axis.title.y = element_text(size = 14),
    legend.position = "none",  # Fjern legenden
    axis.text = element_text(size = 14),
    text = element_text(size = 14, family = "Times New Roman", color = "black"),
    axis.ticks = element_line(size = 0.15, color = "black"),
    axis.ticks.length = unit(-0.15, "cm")  # Negative ticks for "indented" look
  )

# Print plottet
print(plot_polynomials)

