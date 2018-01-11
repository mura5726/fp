N   <- 10000
#デフォルト相関計算する関数
CorrelationOfDefault <- function(probability.default,cor.copula){
  #シングルファクターモデル用の共通ファクター
  common.factor <-  matrix(rep(rnorm(N),2),N)
  #構造型アプローチなので基準化された資産価値出す
  value.asset   <- sqrt(cor.copula) * common.factor + sqrt(1 - cor.copula) * matrix(rnorm(2*N),N)
  #倒産判定
  index.default <- ifelse(pnorm(value.asset) < probability.default,1,0)
  return(cor(index.default)[1,2])
}
#デフォルト確率と資産相関（アセット相関）
probability.default <- seq(0.05,0.95,0.01)
cor.copula          <- seq(0.05,0.95,0.01)
#outerで回すためのラッパー関数
wrapper <- function(x, y, my.fun, ...) {
  sapply(seq(along = x), FUN = function(i)my.fun(x[i], y[i], ...))
}
#各デフォルト確率・資産相関に対してデフォルト相関を計算
cor.default <- outer(probability.default, cor.copula,FUN=wrapper,my.fun=CorrelationOfDefault)
#描画
persp(probability.default, cor.copula, cor.default, theta = -60, phi = 30,ltheta = -30,
      expand = 0.5, col = rainbow(length(probability.default)),shade = 1, ticktype = "detailed",)   
