dnorm

plot(norm(0,1), -1, 1)
xvals <- seq(-4, -2, length=10)               # 領域をx軸方向に10個の多角形(台形)に等分割
dvals <- dnorm(xvals)                       # 対応するグラフの高さ
polygon(c(xvals,rev(xvals)),
        c(rep(0,10),rev(dvals)),col="gray") # 塗りつぶす
title("Title")                              # タイトルを描く
mtext("sub-title", side=4) 
