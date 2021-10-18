# HEADER ----

# Term Paper in Advanced Environmental and Resource Economics (WiSe 2020/21)
# Topic: Industry Presence and Urban Climate Action: Evidence from a Set of Dutch Cities
# Student: Marc Blauert (ID: 605555)

# Purpose of the script: Conduct a descriptive analysis with the selected set of cities.

# Setup ----

rm(list = ls())
pacman::p_load(tidyverse, stringr, RColorBrewer, hrbrthemes, corrplot, QCA, SetMethods, xtable)

# Load set of cities ----
sample_QCA <- "output/tables/sample_QCA.csv"
sample_QCA <- read.csv2(sample_QCA, header = TRUE, stringsAsFactors = FALSE)

QCA_matrix <- "output/tables/QCA_matrix.csv"
QCA_matrix <- read.csv2(QCA_matrix, header = TRUE, stringsAsFactors = FALSE, row.names = 1)

# Cumulative distribution plot for share of city population in COROP region ----

pdf("output/plots/city_corop_pop_ecdf.pdf", height = 10, width = 6)

plot(ecdf(sample_QCA$pop_share_city_of_corop*100),
     xlab="City population / COROP population [%]",
     ylab="Cumulative proportion",
     main="Share of city population in COROP region",
     col="#1B9E77")

dev.off()

# Cumulative distribution plot for conditions with calibration threshold ----

pdf("output/plots/calibration_ecdf.pdf", height = 4, width = 9)

par(mfrow=c(1,3))

plot(ecdf(sample_QCA$EmployedPersons_1000fte_percapita*100),
     xlab="Industry sector FTEs per capita, 2015 [%]",
     ylab="Cumulative proportion",
     main="Industry Presence (IND)",
     col="#1B9E77",
     pch = 8)
abline(v=4.5, col="red")

plot(ecdf(sample_QCA$gpc_mean12_15),
     xlab="GDP per capita, 2012-2015 [EUR]",
     ylab="Cumulative proportion",
     main="Capacity (CAP)",
     col="#7570B3",
     pch = 8)
abline(v=46437, col="red")

plot(ecdf(sample_QCA$floodrisk_city),
     xlab="Flood risk, 2010 [1 (low) - 5 (high)]",
     ylab="Cumulative proportion",
     main="Vulnerability (VUL)",
     col="#D95F02",
     pch = 8)
abline(v=4, col="red")

dev.off()

# Correlation matrix and plot ----

corr_matrix <- cor(QCA_matrix)
write.csv2(corr_matrix,"output/tables/corr_matrix.csv", row.names = TRUE)

pdf("output/plots/corr_plot.pdf", height = 8, width = 8)

par(mfrow=c(1,1))

corrplot(cor(QCA_matrix),   # Correlation matrix
         method = "circle", # Correlation plot method
         type = "lower",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,       # If TRUE (default), adds the diagonal
         tl.col = "black",  # Labels color
         bg = "white",      # Background color
         title = "",        # Main title
         col = NULL)        # Color palette

dev.off()

# Counting types of mitigation plans ----
sample_QCA %>% 
  group_by(mitigation_plan) %>% 
  tally()