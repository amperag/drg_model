#Setup

setwd("~/00 Privat/spiele/drg/model 1")

#Import

data <- read.csv(file = "chart.csv", sep = ";", header = TRUE)

data_selection <- data[455:length(data[,2]),1:2]

#Analysis

require(forecast)

ma <- ma(data_selection[,2], 30)

model <- auto.arima(data_selection[,2])

forecast_period <- 90

model_forecast <- round(median(forecast(model, h = forecast_period)$mean))

png(sprintf("drg_model_%4s.png", Sys.Date()), width = 1600, height = 800)

plot(forecast(model, h = forecast_period), ylab = "Dwarves mining on Hoxxes IV", xlab = sprintf("Days since DRG launch on 28.02.2018 (Model date:%4s)", Sys.Date()), lwd = 1)
lines(ma, col = "red", lwd = 5)
abline(v = c(365,365*2), col = "forestgreen", lty = 2, lwd = 0.5)
abline(h = c(5000, 10000, 15000), col = "grey", lty = 2, lwd = 0.5)
legend(0, 20000, legend=c("Data from SteamDB", "Forecast from ARIMA", "Monthly moving average", "One year elapsed"), col=c("black", "blue", "red", "forestgreen"), lty=c(1, 1, 1, 2), cex=1)
text(800, 250, "Rock and Stone", cex=0.6)
text(800, 12500, sprintf("Average estimated dwarves:%d", model_forecast), col = "blue", cex=0.8)

dev.off()