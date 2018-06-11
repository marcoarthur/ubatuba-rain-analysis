#!/usr/bin/Rscript --vanilla

# read rain data
rain <- read.table(header=T, 'time_series_rain_ubatuba.txt')

# boxplot of monthly raining (1935-2001)
setEPS()
postscript("figures/boxplot.eps", paper="a4")
boxplot(rain[,2:13])

# annualy raining data
annualy <- ts(rain$Anual, start=1935)

# save into figures file
postscript("figures/anual.eps", paper="a4")
plot(annualy)

# calculate the linear model for all months 
for(i in 2:13) { 
    # create time series
    ts_ <- ts(rain[,i], start=1935)

    # time interval
    t <- seq(from=1935, to=(1935 + length(ts_) - 1))

    # calculate linear model for rain prediction
    model <- lm(ts_ ~ t + 1, na.action=na.exclude)

    # plot observed values
    postscript(paste("figures/", "fig_", names(rain[i]), ".eps", sep=''), paper="a4")
    plot(ts_, ylab=paste(names(rain)[i], "(mm)"))

    # plot linear predictor
    lines(t[1:length(model$fitted.values)], model$fitted.values, col="red")
}
dev.off()
