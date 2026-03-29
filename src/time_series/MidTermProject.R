install.packages("readxl")

library(readxl)

load("/Volumes/LaCie/workspace/R/RData/MidTermProject.RData")

google_trend <- read.csv("Data/MidTermProject/googletrend_seoul_commercial.csv")
naver_trend <- read_xlsx("Data/MidTermProject/naversearchtrend_seoul_commercial.xlsx", skip=6)
seoul_commerce <- read_xlsx("Data/MidTermProject/seoul_commercial.xlsx", skip=6)
seoul_sales_est <- read.csv("Data/MidTermProject/seoul_estimate_sales.csv")
seoul_subway_population <- read.csv("Data/MidTermProject/seoul_subway_station_population.csv")
ccsi <- read_xlsx("Data/MidTermProject/ccsi.xlsx", skip=6)

colnames(google_trend) <-c("yyyymm", "restaurent", "hot_place", "attraction", "date_course", "festival")


