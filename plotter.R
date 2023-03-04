require(gridExtra)
library(ggplot2)
library(Hmisc)

library(dplyr)
library(reshape2)
library(moments)
library(ggpmisc)

library(LSTS)

maintheme <-     theme(
  panel.background = element_blank(),
  panel.grid.major.y=element_line(colour="gray85", size=.05),
  panel.grid.major.x= element_blank(),
  axis.line = element_line(colour = NA),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  legend.position = c(0.98, 0.98),
  legend.direction = "horizontal",
  legend.justification = c(1,1),
  legend.title= element_blank(),
  text=element_text(color="black"),axis.text=element_text(color="black")
)


dailyPricesReturns <- function(prices, returns) {
  par(mfrow = c(1, 2), mar=c(5.1, 4.1, 2, 2.1))
  
  plot(prices,
       xlab="Index",
       ylab="Adj. Closing Price (USD)",
       las = 1)
  
  minor.tick(nx=5, ny=0, tick.ratio=0.5, x.args = list())
  plot(returns,
       xlab="Index",
       ylab="log return",
       las = 1)
  minor.tick(nx=5, ny=0, tick.ratio=0.5, x.args = list())
  
  par(mfrow = c(1, 1))
}


doubleCF <- function(cf1, cf2, significance_level, maxLag=25, ylabel="acf", s1Label="1", s2Label="2") {
  
  acf1.df <- with(cf1, data.frame(lag, acf))
  acf2.df <- with(cf2, data.frame(lag, acf))
  
  acf1.df$pow <- s1Label
  acf2.df$pow <- s2Label
  
  dfs <- rbind(acf1.df, acf2.df)
  
  ggplot(data=dfs, mapping=aes(x=lag, y=acf, fill=pow)) +
    geom_bar(width=.5,
             color="gray28",
             size=0.1,
             stat='identity',
             position=position_dodge(.8)
    ) + 
    scale_fill_manual(values=c("gray45","gray80")) +
    xlim(1,maxLag) +
    ylim(-0.10,0.25) + 
    geom_hline(yintercept=significance_level, linetype="dashed", 
               color = "blue", size=0.4) + 
    geom_hline(yintercept=-significance_level, linetype="dashed", 
               color = "blue", size=0.4) + 
    geom_hline(yintercept=0, linetype="solid", 
               color = "black", size=0.5) +
    ylab(ylabel) +
    maintheme
}

acfPacfPlot <- function(returnseries, maxLag=25) {
  
  plot1 <- doubleCF(
    acf(returnseries, lag.max=maxLag, plot = FALSE),
    acf(returnseries^2, lag.max=maxLag, plot = FALSE),
    qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(returnseries^2))),
    ylabel="ACF",
    s1Label="Log Returns",
    s2Label="Sqr Log Returns",
    maxLag=maxLag
  )
  
  plot2 <- doubleCF(
    pacf(returnseries, lag.max=maxLag, plot = FALSE),
    pacf(returnseries^2, lag.max=maxLag, plot = FALSE),
    qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(returnseries^2))),
    ylabel="PACF",
    s1Label="Log Returns",
    s2Label="Sqr Log Returns",
    maxLag=maxLag
  )
  
  grid.arrange(plot1, plot2, ncol=2)
  
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

summaryPlot <- function(series){
  
  df <- data.frame(series)
  
  qq <- ggplot(data.frame(series), aes(sample=series)) +
    stat_qq(size=2.5, color='black',shape=1) + 
    stat_qq_line(color="blue")+ 
    maintheme + 
    ylab("Quantiles of Normal") +
    xlab("Quantiles of Log Returns") +
    theme(
      panel.grid.major.y=element_blank()
    )
  
  #Calculating the summary statistics
  summ <- df %>% 
    summarize(Mean = mean(series),
              Median = median(series),
              SD = sd(series),
              Minimum = min(series),
              Maximum = max(series), 
              Q1= quantile(series, probs = 0.25), 
              Q3= quantile(series, probs = 0.75),
              Skewness=skewness(series),
              Kurtosis=kurtosis(series))
  
  meanVal <- summ$Mean
  sdVal <- summ$SD
  
  summ <- setNames(data.frame(t(summ)), "Value")
  summ$Statistic <- row.names(summ)
  summ <- summ[,c(2,1)]
  summ <- round_df(summ, digits=9)
  
  hist <- ggplot(df, aes(x=series)) +
    geom_histogram(aes(y=..density..), color="black", fill="white")+
    maintheme +
    ylab("Frequency") +
    xlab("Log Return") +
    theme(
      panel.grid.major.y=element_blank()
    )+
    geom_table_npc(
      data = summ,
      label = list(summ),
      npcx = 0.98, npcy = 0.98, hjust = 1, vjust = 1,
      table.theme=ttheme_gtdefault(
        core=list(
          bg_params=list(fill=c("white")),
          fg_params=list(hjust=0, x=0)
        ),
        colhead = list(bg_params = list(fill = "white")))) +
    stat_function(fun = dnorm, args = list(mean = meanVal, sd = sdVal), color="blue")
  
  grid.arrange(qq, hist, ncol=2)
}


oldCombined <- function(
    series,
    seriesReturns,
    ylim1=c(50,500),
    ylim2=c(-0.7,0.29),
    ylab1=seq(0, 300, by = 50),
    ylab2=seq(-0.15, 0.3, by = 0.15),
    legendLabel,
    mainn,
    roundDigits=6) {
  
  # combined plot
  par(mar = c(5, 5, 3, 5))
  plot(series, type ="l",
       col = "black", ylim=ylim1, yaxt = "n",
       ylab = "",
       main=mainn,
       xlab="Index")
  
  
  axis(side = 2, at=ylab1, las = 1)
  mtext("Price (USD)", side = 2, line = 3, adj=0.22, padj=-0.5)
  
  
  par(new = TRUE)
  
  
  plot(seriesReturns, type = "l", ylim=ylim2, xaxt = "n", yaxt = "n",
       ylab = "", xlab = "", col = "gray65")
  # abline(h=0, col="black", lty=2)
  
  axis(side = 4, at=ylab2, las = 1)
  
  
  mtext("log-return", side = 4, line = 3, adj=0.85, padj=0.5)
  
  legend("topleft",
         text.width=c(40),
         bty = "n",
         legend=legendLabel,
         col=c("black", "gray65"),
         lwd=2,
         cex = 1,
         horiz=TRUE)
}


residualPlots <- function(
    residualSeries
    ) {
  
  resDf <- data.frame(resid = residualSeries, index = 2:(length(residualSeries)+1))
  
  qq <- ggplot(resDf, aes(y=resid, x=index)) + 
    geom_line() +
    ylab("Standardized Residual") +
    maintheme
  
  
  box <- Box.Ljung.Test(residualSeries, lag=20) +
    maintheme
  
  
  grid.arrange(qq, box, ncol=2)
}
