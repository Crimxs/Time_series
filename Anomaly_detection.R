getwd()
setwd("C:\\Users\\Crimxs\\Documents\\Praxis\\PraxisDoc\\Term_2\\TimeSeries_Forecasting\\Session")
install.packages("outliers")
library(githubinstall)
library(outliers)
library(lubridate)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

m_data <- read.csv("Medium.csv")

View(m_data)

sapply(m_data, class)

m_data$published <- ymd_hms(m_data$published)


res = AnomalyDetectionTs(m_data, max_anoms=0.02, alpha = 0.5, direction='both', plot=TRUE)
res$plot

grubbs.test(m_data$posts, type = 11) # checks only single value



