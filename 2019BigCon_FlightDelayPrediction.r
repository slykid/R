tryCatch(
  setwd("C:/workspace/R"),
  error = function(e) {
    setwd("D:/workspace/R")
  }
)

afsnt <- read.csv("Data/AFSNT.csv", header=T, stringsAsFactors = F)
afsnt_dly <- read.csv("Data/AFSNT_DLY.csv", header=T, stringsAsFactors = F)
sfsnt <- read.csv("Data/sfsnt.csv", header=T, stringsAsFactors = F)

sfsnt[sfsnt$FSD %in% unique(sfsnt$FSD),]
