#Garchモデルによる時系列分析パッケージ
# install.packages("fGarch")
library(fGarch)
#各種webサイトからデータを取ってこれるパッケージ
# install.packages("fImport")
library(fImport)
#S&P500 Indexデータを取得 & リターン系列(算術・日次)に変換
sp500.index  <- fredSeries("SP500",from = "2008-01-01")
sp500.return <- sp500.index / lag(sp500.index)-1
sp500.return <- sp500.return[!is.na(sp500.return),]
#Garchモデルによるフィッティングの実行
sp500.fit    <- garchFit( ~ garch(1, 1), data = sp500.return, trace = FALSE)
plot(cbind(sp500.return,volatility=volatility(sp500.fit, type = "sigma")))

#株価等の時系列データを美しくPLOT＆テクニカル指標を追記できるパッケージ
# install.packages("quantmod")
library(quantmod)
#やほーの株価データ取得
# getSymbols('yahoo')
# ?getSymbols()

# data.env <- new.env()
# getSymbols("YHOO", env=data.env)

# getSymbols(c('QQQ','SPY'))   


getSymbols("998407", src="yahooj")
candleChart(to.weekly(YJ998407),subset='2009',theme='white.mono',TA="addVo();addEMA();addRSI()",up.col=0)

YJ998407
#アメリカの株価データが入っているパッケージ
# install.packages("fEcofin")
# library(fEcofin)
#ポートフォリオ分析ができるパッケージ
library(fPortfolio)
#アメリカの株式データ取得
# data(berndtInvest)
# berndt <- as.timeSeries(YJ998407)

data1 = read.csv("history.csv")

datats = as.timeSeries(data1)
#マーケットインデックスの値と無リスク金利を除く
# data.return <- berndt[, -c(10, 17)]
#効率的フロンティアを計算
frontier <- portfolioFrontier(data1)
tailoredFrontierPlot(frontier)
#接点ポートの計算
portfolio.tangency <- tangencyPortfolio(data.return)    
#投資ウェイト&リスクウェイトの表示
weightsPie(portfolio.tangency)
covRiskBudgetsPie(portfolio.tangency)

