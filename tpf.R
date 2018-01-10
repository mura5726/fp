tailoredFrontierPlot <-
  function(object,
           return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
           mText = NULL, col = NULL, xlim = NULL, ylim = NULL, 
           twoAssets = FALSE, sharpeRatio = TRUE, 
           title = TRUE, ...)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Creates an easy to use tailored frontier plot
    
    # Arguments:
    #   object - a portfolio object
    #   mtext - not used
    
    # FUNCTION:
    
    # 1. Plot the Frontier, add margin text, grid and ablines:
    offset <- 0.10
    risk <- match.arg(risk)
    return <- match.arg(return)
    
    # x - Range:
    if (is.null(xlim)) {
      if (risk == "Cov") {
        xmax <- max(sqrt(diag(getCov(object))))
      }
      if (risk == "Sigma") {
        xmax <- max(sqrt(diag(getSigma(object))))
      }
      if (risk == "CVaR") {
        alpha <- getAlpha(object)
        quantiles <- colQuantiles(getSeries(object), prob = alpha)
        n.max <- which.max(-quantiles)
        r <- getSeries(object)[, n.max]
        r <- r[r < quantiles[n.max]]
        xmax <- -mean(r)
      }
      if (risk == "VaR") {
        xmax <- max(-colQuantiles(getSeries(object), prob = alpha))
      }
      xlim <- c(0, xmax)
      Xlim <- c(xlim[1]-diff(xlim)*offset, xlim[2]+diff(xlim)*offset)
    } else {
      Xlim <- xlim
    }
    
    # y - Range:
    if (is.null(ylim)) {
      if (return == "mean") {
        ylim <- range(getMean(object))
      } else {
        ylim <- range(getMu(object))
      }
      Ylim <- c(ylim[1]-diff(ylim)*offset, ylim[2]+diff(ylim)*offset)
    } else {
      Ylim = ylim
    }
    
    # Frontier Plot:
    frontierPlot(object, labels = FALSE,
                 return = return, risk = risk, auto = FALSE, 
                 xlim = Xlim, ylim = Ylim, title = title, pch = 19, ...)
    
    # Add Grid:
    grid()
    
    # Add Center-Hair Cut:
    abline(h = 0, col = "grey")
    abline(v = 0, col = "grey")
    
    # 2. Add minimum risk (variance) Portfolio Point:
    data <- getData(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    mvPortfolio <- minvariancePortfolio(data, spec, constraints)
    minvariancePoints(object, return = return, risk = risk, auto = FALSE,
                      pch = 19, col = "red")
    
    # 3. Add Tangency Portfolio Point and Tangency Line:
    tangencyPoints(object, return = return, risk = risk, auto = FALSE,
                   pch = 19, col = "blue")
    tangencyLines(object, return = return, risk = risk, auto = FALSE,
                  col = "blue")
    
    # 4. Add Equal Weights Portfolio:
    # xy <- equalWeightsPoints(object, return = return, risk = risk, 
                             # auto = FALSE, pch = 15, col = "grey")
    # text(xy[, 1]+diff(xlim)/20, xy[, 2]+diff(ylim)/20, "EWP",
    #      font = 2, cex = 0.7)
    # 
    # 5. Add all Assets Points:
    if (is.null(col)) col = rainbow(6)
    xy <- singleAssetPoints(object, return = return, risk = risk, 
                            auto = FALSE, cex = 1.5,
                            col = col, lwd = 2)
    text(xy[, 1]+diff(xlim)/20, xy[, 2]+diff(ylim)/20,
         rownames(xy), font = 2, cex = 0.7)
    
    # 6. Add optionally all Two Assets  Lines
    if (twoAssets) {
      twoAssetsLines(object, return = return, risk = risk, auto = FALSE,
                     lty = 3, col = "grey")
    }
    
    # 6. Add Sharpe Ratio Line:
    if(sharpeRatio) {
      sharpeRatioLines(object, return = return, risk = risk, auto = FALSE,
                       col = "orange", lwd = 2)
    }
    
    # Add Rmetrics - Do not Remove!
    mtext("Rmetrics", adj=0, side=4, cex=0.7, col="darkgrey")
    
    # Return Value:
    invisible(list(object=object, xlim=Xlim, ylim=Ylim))
  }
