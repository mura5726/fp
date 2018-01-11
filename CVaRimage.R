# #dnorm
# 
# x <- seq(-40, 40, 0.05)
# plot(x,dnorm(x,mean=0,sd=10),type = "n")
# xvals <- seq(-4, -2, length=10)               # 領域をx軸方向に10個の多角形(台形)に等分割
# dvals <- dnorm(xvals)                       # 対応するグラフの高さ
# polygon(c(xvals,rev(xvals)),
#         c(rep(0,10),rev(dvals)),col="gray") # 塗りつぶす
# title(main = "", xlab = "年率運用利回り(%)", ylab = "頻度")                            # タイトルを描く
# mtext("sub-title", side=4) 
# 
# 
# x <- seq(-40, 40, 0.5)
# plot(x,dnorm(x, mean=0, sd=5), type="n")
# curve(dnorm(x, mean=0, sd=10), type="l",add=T)

plot(dnorm,-4,4, xlab = "年率運用利回り(%)", ylab = "密度", xlim = c(-4.5,4.5))
xvals <- seq(-4, -2, length=10)               # 領域をx軸方向に10個の多角形(台形)に等分割
dvals <- dnorm(xvals)                       # 対応するグラフの高さ
polygon(c(xvals,rev(xvals)),
        c(rep(0,10),rev(dvals)),col="gray") # 塗りつぶす

# abline(v = -2, lty=2)
segments(-2, 0, -2, 0.1,lty=2)
segments(-2.5, 0, -2.5, 0.08,lty=2)
segments(-2, 0.135, -2, 0.2,lty=2)
segments(-4, 0, -4, 0.2,lty=2)

text(-2,0.12,"VaR")
text(-2.5,0.10,"CVaR")
text(-4,0.22,"最大損失")
text(-3,0.16,"β", col = "blue")
text(-3,0.18,"確率", col = "blue")
arrows(-2.7, 0.16, -2, 0.16, col="blue",length = 0.1)
arrows(-3.3, 0.16, -4, 0.16, col="blue",length = 0.1)
# title(main = "", xlab = "年率運用利回り(%)", ylab = "密度")                            # タイトルを描く


# plot(rnorm(50), rnorm(50), xlim=c(-3,6), ylim=c(-3,3), axes = F, ann=F)
# axis(1, pos = 0, at = -3:6, adj = 0, col = 2)                      # 赤で X 軸を描く
# axis(2, pos = 0, at = -3:6, adj = 1, las = 2)                      # 黒で Y 軸を描く
# box() 
# legend(2, 3, paste("sin(",6:9,"x)"), col=6:9,    
#        pch=3, ncol=2, cex=1.1, pt.bg="pink")                       # (5,9) に凡例を描画
# polygon(3:6, c(-2,-1,-2,-1), density=c(10, 20), angle=c(-45, 45))
# arrows(5, -1, 4, 2, col="blue")             

plot(dnorm,-4,4, xlab = "年率運用利回り(%)", ylab = "密度", xlim = c(-4.5,4.5))
xvals <- seq(-4, -2, length=10)               # 領域をx軸方向に10個の多角形(台形)に等分割
dvals <- dnorm(xvals)                       # 対応するグラフの高さ
polygon(c(xvals,rev(xvals)),
        c(rep(0,10),rev(dvals)),col="gray") # 塗りつぶす

# abline(v = -2, lty=2)
segments(-2, 0, -2, 0.2,lty=2)
# segments(-2.5, 0, -2.5, 0.08,lty=2)
# segments(-2, 0.135, -2, 0.2,lty=2)
segments(-4, 0, -4, 0.2,lty=2)

# text(-2,0.12,"VaR")
# text(-2.5,0.10,"CVaR")
text(-4,0.22,"最大損失")
text(-3,0.14,"CVaR(p)")
text(-3,0.20,"p%点以下の")
text(-3,0.18,"条件付き確率")
text(-1.48,0.065,"p%点")
# text(-3,0.18,"確率", col = "blue")
arrows(-3.3, 0.16, -2, 0.16,length = 0.1)
arrows(-3.3, 0.16, -4, 0.16,length = 0.1)
arrows(-1.5, 0.05, -1.96, 0.00,length = 0.05)
