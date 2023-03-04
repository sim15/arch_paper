library(zoo)
library(dse)
library(LSTS)
library(fGarch)
library(extrafont)
library(ggplot2)
library(tseries)
font_install("fontcm")
loadfonts()

setPdf <- function(fname, w, h) {
  pdf(file=fname, height=h, width=w, family="CM Roman")
  par(
    family="CM Roman"
  )
  
  # pdf(file=fname, height=h, width=w)
}

# plotting functions
source("plotter.R")

# load raw data
nasdaq <- read.csv("^IXIC (1).csv", header=T)
ts.nasdaq <- ts(nasdaq$Adj.Close)
nasdaq.logreturns <- diff(log(ts.nasdaq))

# plot of monthly prices and returns (two graphs)
{
setPdf("./nasdaqFigs/monthlyPricesLogReturns.pdf", 8, 3)
dailyPricesReturns(ts.nasdaq, nasdaq.logreturns)
dev.off()
}

# plot acf and pacf corelogram
{
  setPdf("./nasdaqFigs/acfpacf.pdf", 1.2*9, 1.2*3)
  acfPacfPlot(nasdaq.logreturns, maxLag=40)
  dev.off()
}

# plot summary statistics + Q-Q plot
{
  setPdf("./nasdaqFigs/summaryStats.pdf", 1.7*8, 1.7*3)
  summaryPlot(nasdaq.logreturns)
  dev.off()
}

# plot combined returns and price of data
{
  setPdf("./nasdaqFigs/combinedReturns.pdf", 1.2*9, 1.2*5)
  oldCombined(ts.nasdaq, nasdaq.logreturns,
              ylim1=c(0,23000),
              ylim2=c(-1,0.2),
              ylab1=seq(0, 15000, by = 3000),
              ylab2=seq(-0.3, 0.3, by = 0.15),
              legendLabel=c("NASDAQ", "NASDAQ Returns"),
              mainn="Monthly Closing Price and Returns of NASDAQ (1985−2023)",
              roundDigits=3)
  dev.off()
}

# ADF test results for stationarity
adf.test(nasdaq.logreturns)

# all model fits (normal and Student's t-distribution)
fit.arch1.norm=garchFit(nasdaq.logreturns~garch(1,0),data=nasdaq.logreturns,trace=F)
fit.arch1.std=garchFit(nasdaq.logreturns~garch(1,0),data=nasdaq.logreturns,trace=F, cond.dist="std")

fit.arch2.norm=garchFit(nasdaq.logreturns~garch(2,0),data=nasdaq.logreturns,trace=F)
fit.arch2.std=garchFit(nasdaq.logreturns~garch(2,0),data=nasdaq.logreturns,trace=F, cond.dist="std")

fit.arch3.norm=garchFit(nasdaq.logreturns~garch(3,0),data=nasdaq.logreturns,trace=F)
fit.arch3.std=garchFit(nasdaq.logreturns~garch(3,0),data=nasdaq.logreturns,trace=F, cond.dist="std")

fit.garch11.norm=garchFit(nasdaq.logreturns~garch(1,1),data=nasdaq.logreturns,trace=F)
fit.garch11.std=garchFit(nasdaq.logreturns~garch(1,1),data=nasdaq.logreturns,trace=F, cond.dist="std")

fit.garch12.norm=garchFit(nasdaq.logreturns~garch(1,2),data=nasdaq.logreturns,trace=F)
fit.garch12.std=garchFit(nasdaq.logreturns~garch(1,2),data=nasdaq.logreturns,trace=F, cond.dist="std")

fit.garch21.norm=garchFit(nasdaq.logreturns~garch(2,1),data=nasdaq.logreturns,trace=F)
fit.garch21.std=garchFit(nasdaq.logreturns~garch(2,1),data=nasdaq.logreturns,trace=F, cond.dist="std")


# plot residual summary of model
{
  setPdf("./nasdaqFigs/standardResidualSummary.pdf", 0.7*14, 0.7*5)
  residualPlots(residuals(fit.garch11.std, standardize=TRUE))
  dev.off()
}

# fit model and predictions results
summary(fit.garch11.std)

predict(fit.garch11.norm, n.ahead=6, plot = T)
predict(fit.garch11.std, n.ahead=6, plot = T)

plot(fit.garch11.std)
fit.garch11.std
