
Data <- read_csv("FJ/FJRF.csv")
Data <- read_csv("FJ/FJSD.csv")
Data <- read_csv("FJ/FJJA.csv")
Data <- read_csv("FJ/FJLY.csv")
Data <- read_csv("FJ/FJFP.csv")

sum(Data$E)
sum(Data$O)
sum(length(unique(Data$age)))
sum(length(unique(Data$duration)))
