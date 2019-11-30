#setwd("C:/workspace/R")
setwd("D:/workspace/R")
load("C:/workspace/R/building_total_info_master.RData")

#install.packages("foreign")
library("foreign")

# 데이터 전처리
date <- 201907
city_num <- c(11, 26:31, 36, 41:48, 50)
for(i in city_num){
  tryCatch(data <- read.dbf(paste0("F_FAC_BUILDING_",as.character(i),"_",date,".dbf"), as.is=TRUE),
      error = function(e) data <- read.dbf(paste0("F_FAC_BUILDING_",as.character(i),"_"(date-1),".dbf"), as.is=TRUE)
      )
  write.csv(data,paste0("F_FAC_BUILDING_",as.character(i),"_",date,".csv"), row.names = FALSE, fileEncoding = "cp949")
  print(paste0(i,"번 데이터가 완료되었습니다."))
}

# 통합본 생성
isFirst <- T
for(i in city_num) {
  tryCatch(data <- read.csv(paste0("Data/F_FAC_BUILDING_",as.character(i),"_",date,".csv"), as.is=TRUE),
           error = function(e) data <- read.csv(paste0("Data/F_FAC_BUILDING_",as.character(i),"_",(date-1),".csv"), as.is=TRUE)
  )
  
  CityCode <- rep(i,length(row.names(data)))
  data <- cbind(data,CityCode)
  
  if (isFirst == T) {
    totalData <- data
    isFirst <- F
  }
  else {
    totalData <- rbind(totalData, data)
  }
  print(paste0(i,"번 데이터가 완료되었습니다."))
}
write.csv(totalData,paste0("Data/F_FAC_BUILDING_TOTAL_",date,".csv"), row.names = FALSE, fileEncoding = "cp949")