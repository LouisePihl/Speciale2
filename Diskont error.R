##############################################
##            Diskonteringsfejl             ##
##############################################

# Definerer variabler
stepsize <- c(12, 18, 24, 30, 36, 42)  # Stepsize værdier
produkttype <- c("Product 1", "Product 2")  # Produkttyper

# Data for de to produkttyper
values_1 <- c(814015.2, 805382.9, 801679.6, 799036.7, 797527.9, 796416.3)
values_2 <- c(890877.8, 882526.2, 878452.6, 876009.1, 874501.9, 873217.0)

# Opret dataframen
df <- data.frame(
  Produkttype = rep(produkttype, each = length(stepsize)),
  Stepsize = rep(stepsize, times = length(produkttype)),
  Værdi = c(values_1, values_2)
)

# Vis dataframe
print(df)


# Opret ggplot
library(ggplot2)
ggplot(df, aes(x = Stepsize, y = Værdi, color = Produkttype)) +
  geom_line(size = 1) +         # Linjer for hvert produkt
  geom_point(size = 2) +        # Punkter ved hvert stepsize-punkt
  labs(
    title = "",
    x = "Stepsize",
    y = "Reserve",
    color = ""
  ) + theme_gray()




#-------------Error i pct ---------------

(values_1[1]-values_1[6])/values_1[1]

(values_1[1]-values_1[3])/values_1[1]

(values_1[1]-values_1[4])/values_1[1]

(values_1[1]-values_1[5])/values_1[1]

(values_1[6]-values_1[1])/values_1[6]


(values_2[1]-values_2[2])/values_2[1]

(values_2[1]-values_2[3])/values_2[1]

(values_2[1]-values_2[4])/values_2[1]

(values_2[1]-values_2[5])/values_2[1]

(values_2[1]-values_2[6])/values_2[1]



