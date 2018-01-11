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