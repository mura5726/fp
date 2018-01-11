#windows
# install.packages("fPortfolio")
# install.packages("maptools")
# install.packages("vars")
# install.packages("VineCopula")
library(fPortfolio)

library(maptools)
library(vars)
library(VineCopula)
# setwd("/Users/kmurase/gitrepos/fp")
setwd("C:/Users/mura5/Documents/fp")

rm(list=ls(all=TRUE))

weightsBefore <- c(0.6125, 0.1325, 0.1225, 0.1325)
weightsAfter <- c(0.35, 0.25, 0.15, 0.25)


#fPortfolio
#各ポートフォリオの平均分散算出
history <- read.csv("history.csv", header = TRUE)
hist <- history[, -1]
hist <- hist * 12

dat <- as.timeSeries(hist)
dat <- cbind(dat[,2], dat[,1], dat[,4], dat[,3])


#元データを一様分布[0, 1]に変換
dimention <- 4
x <- dat
N <- length(x[, 1])
y <- matrix(0, nrow = N, ncol = dimention)

for(i in 1:dimention){
  sortlist <- order(x[, i])
  temp0 <- ecdf(x[sortlist, i])
  y[, i] <- temp0(x[, i])
}


y[y == 1] <- (1 - 1 / 2 / N)


#コピュラ推定
est <- RVineStructureSelect(y, progress = TRUE)
contour(est)

plot(y[,4],y[,2])
