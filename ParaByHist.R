
# miss weights
weightsBefore <- c(0.6125, 0.1325, 0.1225, 0.1325)
weightsAfter <- c(0.35, 0.25, 0.15, 0.25)

#fPortfolio
#各ポートフォリオの平均分散算出
history <- read.csv("history.csv", header = TRUE)
hist <- history[, -1]
hist <- hist * 12 * 100

dat <- as.timeSeries(hist)

dat <- cbind(dat[,2], dat[,1], dat[,4], dat[,3])

# head(dat)

# #期間をわける
# dat1 <- dat[1:76, ]       #2001/4-2007/7
# dat2 <- dat[77:105, ]     #2007/8-2009/12
# dat3 <- dat[106:192, ]    #2010/1-2017/3

#期間をわける(Var.2)
dat1 <- dat[1:76, ]       #2001/4-2007/7
dat2 <- dat[77:100, ]     #2007/8-2009/7
dat3 <- dat[101:192, ]    #2010/8-2017/3

dat1plus3 <- rbind(dat1,dat3)
dat_list = list(dat,dat1,dat2,dat3,dat1plus3)

#change hist
##################
# hist = hist3
##################

SigmaPara = matrix(0,4,5)
colnames(SigmaPara) = c("ALL","1","2","3","1+3")
rownames(SigmaPara) = c("Sigma_beforeY","Sigma_beforeX","Sigma_afterY","Sigma_afterX")

CVaRPara = matrix(0,4,5)
colnames(CVaRPara) = c("ALL","1","2","3","1+3")
rownames(CVaRPara) = c("CVaR_beforeY","CVaR_beforeX","CVaR_afterY","CVaR_afterX")


# set hist

#Sigma

getSigmaPara = function(x){
  for(i in 1:5){
  
    hist = x[[i]]
    
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
    
    # CVaRBefore <- abs(cvarRisk(dat, weightsBefore, alpha = 0.05))
    # CVaRAfter  <- abs(cvarRisk(dat, weightsAfter, alpha = 0.05))
    # 
    
    SigmaPara[,i] = c(beforeY,beforeX,afterY,afterX)
    
  }
  return(SigmaPara)
}

#CVaR
# x = hist_list
# wb = weightsBefore
# wa = weightsAfter

getCVaRPara = function(x, wb, wa){
  for(i in 1:5){
    # i = 2
    hist = x[[i]]
    
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
    # beforeX <-((0.6125)^2 * varBB + (0.1325)^2 * varAA + 
    #              (0.1225)^2 * varDD + (0.1325)^2 * varCC + 
    #              2 * 0.6125 * 0.1325 * varAB + 
    #              2 * 0.1325 * 0.1225 * varDA + 
    #              2 * 0.1325 * 0.6125 * varBC + 
    #              2 * 0.6125 * 0.1225 * varBD + 
    #              2 * 0.1325 * 0.1325 * varAC + 
    #              2 * 0.1225 * 0.1325 * varCD)^0.5
    
    afterY <- 0.35 * meanB + 0.25 * meanA + 0.15 * meanD + 0.25 * meanC
    # afterX <-((0.35)^2 * varBB + (0.25)^2 * varAA +
    #             (0.15)^2 * varDD + (0.25)^2 * varCC + 
    #             2 * 0.35 * 0.25 * varAB + 
    #             2 * 0.25 * 0.15 * varDA + 
    #             2 * 0.25 * 0.35 * varBC + 
    #             2 * 0.35 * 0.15 * varBD + 
    #             2 * 0.25 * 0.25 * varAC + 
    #             2 * 0.15 * 0.25 * varCD)^0.5
    # 
    CVaRBefore <- abs(cvarRisk(hist, wb, alpha = 0.05))
    CVaRAfter  <- abs(cvarRisk(hist, wa, alpha = 0.05))

    
    CVaRPara[,i] = c(beforeY,CVaRBefore,afterY,CVaRAfter)
    
  }
  return(CVaRPara)
}

getSigmaPara(dat_list)
getCVaRPara(dat_list, weightsBefore, weightsAfter)

#writecsv

write.csv(getSigmaPara(dat_list),"SigmaPara.csv")
write.csv(getCVaRPara(dat_list, weightsBefore, weightsAfter),"CVaRPara.csv")

