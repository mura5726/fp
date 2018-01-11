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

#rewrite fportfolio
# source("fppack.R")

weightsBefore <- c(0.6125, 0.1325, 0.1225, 0.1325)
weightsAfter <- c(0.35, 0.25, 0.15, 0.25)

#fPortfolio
#各ポートフォリオの平均分散算出
history <- read.csv("history.csv", header = TRUE)
hist <- history[, -1]
hist <- hist * 12

#期間をわける
# hist1 <- hist[1:76, ]       #2001/4-2007/7
# hist2 <- hist[77:105, ]     #2007/8-2009/12
# hist3 <- hist[106:192, ]    #2010/1-2017/3

#期間をわける(Var.2)
hist1 <- hist[1:76, ]       #2001/4-2007/7
hist2 <- hist[77:100, ]     #2007/8-2009/7
hist3 <- hist[101:192, ]    #2010/8-2017/3


#change hist
##################
# hist = hist
##################

A <- hist$国内株式
B <- hist$国内債券
C <- hist$外国株式
D <- hist$外国債券


meanA <- mean(hist$国内株式)
meanB <- mean(hist$国内債券)
meanC <- mean(hist$外国株式)
meanD <- mean(hist$外国債券)

sdA <- sd(hist$国内株式)
sdB <- sd(hist$国内債券)
sdC <- sd(hist$外国株式)
sdD <- sd(hist$外国債券)

varAA <- var(hist$国内株式)
varBB <- var(hist$国内債券)
varCC <- var(hist$外国株式)
varDD <- var(hist$外国債券)

cor(hist)

varAB <- var(hist$国内株式, hist$国内債券)
varBC <- var(hist$国内債券, hist$外国株式)
varDA <- var(hist$外国債券, hist$国内株式)
varAC <- var(hist$国内株式, hist$外国株式)
varBD <- var(hist$国内債券, hist$外国債券)
varCD <- var(hist$外国株式, hist$外国債券)

# sdAB <- sqrt(cor(A, B))
# sdBC <- sqrt(var(B, C))
# sdDA <- sqrt(var(D, A))
# sdAC <- sqrt(var(A, C))
# sdBD <- sqrt(var(B, D))
# sdCD <- sqrt(var(C, D))

beforeY <- 0.6125 * meanB + 0.1325 * meanA + 0.1225 * meanD + 0.1325 * meanC
beforeX <-((0.6125)^2 * varBB + (0.1325)^2 * varAA + 
             (0.1225)^2 * varDD + (0.1325)^2 * varCC + 
             2 * 0.6125 * 0.1325 * varAB + 
             2 * 0.1325 * 0.1225 * varDA + 
             2 * 0.1325 * 0.6125 * varBC + 
             2 * 0.6125 * 0.1225 * varBD + 
             2 * 0.1325 * 0.1325 * varAC + 
             2 * 0.1225 * 0.1325 * varCD)^0.5

afterY <- 0.35 * meanB + 0.25 * meanA + 0.15 * meanD + 0.25 * meanC
afterX <-((0.35)^2 * varBB + (0.25)^2 * varAA + 
            (0.15)^2 * varDD + (0.25)^2 * varCC + 
            2 * 0.35 * 0.25 * varAB + 
            2 * 0.25 * 0.15 * varDA + 
            2 * 0.25 * 0.35 * varBC + 
            2 * 0.35 * 0.15 * varBD + 
            2 * 0.25 * 0.25 * varAC + 
            2 * 0.15 * 0.25 * varCD)^0.5

#いこーるうぇいと
# beforeY <- 0.25 * meanA + 0.25 * meanB + 0.25 * meanC + 0.25 * meanD
# beforeX <-((0.25)^2 * varAA + (0.25)^2 * varBB + 
#              (0.25)^2 * varCC + (0.25)^2 * varDD + 
#              2 * 0.25 * 0.25 * varAB + 
#              2 * 0.25 * 0.25 * varBC + 
#              2 * 0.25 * 0.25 * varDA + 
#              2 * 0.25 * 0.25 * varAC + 
#              2 * 0.25 * 0.25 * varBD + 
#              2 * 0.25 * 0.25 * varCD)^0.5


dat <- as.timeSeries(hist)

dat <- cbind(dat[,2], dat[,1], dat[,4], dat[,3])

source("frontierPlot1.R")

###標準偏差
##全期間
conditions <- portfolioSpec()
setNFrontierPoints(conditions) <- 100
efficientFrontier <- portfolioFrontier(dat, conditions)
cols <- c("magenta", "cyan", "green", "yellow", "blue", "red")

frontierPlot1(efficientFrontier, risk = c("Sigma"), pch = 19, xlim = c(0,1), ylim = c(0,0.1))
frontierPlot1(efficientFrontier, risk = c("Sigma"), pch = 19)

#for hist1
# frontierPlot1(efficientFrontier, risk = c("Sigma"), pch = 19, xlim = c(0,0.55))

#for hist3
# frontierPlot1(efficientFrontier, risk = c("Sigma"), pch = 19, ylim = c(-0.02,0.15))

# tailoredFrontierPlot(efficientFrontier, risk = c("Sigma"), sharpeRatio = F)
# 
# tailoredFrontierPlot(efficientFrontier, risk = c("Sigma"))

# ?tailoredFrontierPlot

points(beforeX, beforeY, col = cols[5], pch = 18, cex = 1.5)
points(afterX, afterY, col = cols[6], pch = 18, cex = 1.5)

singleAssetPoints(efficientFrontier, pch = 18, cex = 1.5, col = cols[1:4])

labels <- c("変更前", "変更後", colnames(dat))
legend("topleft", legend = labels, col = c(cols[5], cols[6], cols[1:4]), pch = 18)
# plot(efficientFrontier)


# weightsPlot(efficientFrontier)

# #全資産5%以上保有
# weightConstraints <- c("minW[1:4] = c(0.05,0.05,0.05,0.05)")
# efficientFrontier <- portfolioFrontier(dat, conditions, c(weightConstraints), xlab = "")


#CVaR
# dat <- as.timeSeries(hist)

dat <- as.timeSeries(hist)

dat <- cbind(dat[,2], dat[,1], dat[,4], dat[,3])

# EW
# weightsBefore <- c(0.25, 0.25, 0.25, 0.25)
# weightsAfter <- c(0.25, 0.25, 0.25, 0.25)

CVaRBefore <- abs(cvarRisk(dat, weightsBefore, alpha = 0.05))
CVaRAfter  <- abs(cvarRisk(dat, weightsAfter, alpha = 0.05))

conditions <- portfolioSpec(list(type = "CVaR",
                                 optimize = "minRisk",
                                 estimator = "covEstimator",
                                 tailRisk = list(),
                                 params = list(alpha = 0.04)))
setNFrontierPoints(conditions) <- 100
efficientFrontier <- portfolioFrontier(dat, conditions)

frontierPlot(efficientFrontier, pch = 19, xlim = c(0,1.4), ylim = c(0,0.1))
frontierPlot(efficientFrontier, pch = 19)

singleAssetPoints(efficientFrontier, pch = 18, cex = 1.5, col = cols[1:4])



points(CVaRBefore, beforeY, col = cols[5], pch = 18, cex = 1.5)
points(CVaRAfter, afterY, col = cols[6], pch = 18, cex = 1.5)


labels <- c( "変更前", "変更後", colnames(dat))
legend("topleft", legend = labels, col = c(cols[5:6],cols[1:4]), pch = 18)

###################################################################################
# plot(efficientFrontier)

# weightsPlot(efficientFrontier)

minvariancePortfolio(dat)


return = dat%*%weightsBefore
colnames(return) = "return"
# par(las=1, xaxs="i", yaxs="i", cex.main=3, cex.lab=1.1, cex.axis=1.1)
# par(las=1, family="Century gothic", xaxs="i", yaxs="i", cex.main=3, cex.lab=1.1, cex.axis=1.1, font.lab=2, font.axis=2)
# hist(return[return>-0.5],breaks=seq(-1,1,0.05), xlab="return", ylim=c(0,45), col="#993435")
# box()
# hist(return[return<-0.5],breaks=seq(-1,1,0.05), xlab="return", ylim=c(0,45), col="#edae00", add=T)

hist1 <- hist(return[return>(-0.5)], col ="#0000ff40" , border = "#ff00ff", breaks = seq(-1,1,0.05),main ="hist of return",xlab ="return",cex.main=2)
hist1 <- hist(return[return<(-0.5)], col ="#ff00ff40" , border = "#0000ff", breaks = seq(-1,1,0.05),add = T)
abline(v = -0.5, lty = 2)

hist(return[return<(-0.5)])
# #LPM
# dat <- as.timeSeries(hist)
# weightsBefore <- c(0.1325, 0.6125, 0.1325, 0.1225)
# weightsAfter <- c(0.25, 0.35, 0.25, 0.15)
# LPMBefore <- abs(cvarRisk(dat, weightsBefore, alpha = 0.05))
# LPMAfter  <- abs(cvarRisk(dat, weightsAfter, alpha = 0.05))
# 
# conditions <- portfolioSpec(model = list(type = "LPM",
#                                          optimize = "minRisk",
#                                          estimator = "lpmEstimator",
#                                          params = list(alpha = 0.05, a = 2)),
#                             portfolio = list(weights = NULL,
#                                              targetReturn = .038))
# setNFrontierPoints(conditions) <- 100
# efficientFrontier <- portfolioFrontier(dat, conditions)
# # efficientFrontier <- portfolioFrontier(dat, portfolioSpec())
# 
# frontierPlot(efficientFrontier, pch = 19, risk = c("Sigma"))
# # tailoredFrontierPlot(efficientFrontier, pch = 19, risk = c("Sigma"))
# 
# singleAssetPoints(efficientFrontier, pch = 18, cex = 1.5, col = cols[1:4])
# 
# points(CVaRBefore, beforeY, col = 1, pch = 18)
# points(CVaRAfter, afterY, col = 2, pch = 18)
# 
# 
# labels <- c(colnames(dat), "変更前", "変更後")
# legend("topleft", legend = labels, col = cols, pch = 18)
# 
# plot(efficientFrontier, 4)
# 
# points(CVaRBefore, beforeY, col = 1, pch = 18)
# points(CVaRAfter, afterY, col = 2, pch = 18)
# labels <- c(colnames(dat), "変更前", "変更後")
# legend("topleft", legend = labels, col = cols, pch = 18)









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

