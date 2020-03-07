#Setup

#setwd("...")

#Import

data <- read.csv(file = "chart.csv", sep = ";", header = TRUE, col.names = c("Date", "Players"))

data_selection <- data[455:length(data[,2]),1:2]

#The 455th entry is the early access start on Steam on the 28th of February 2018.

data_selection$Date <- substr(data_selection$Date, 1, 10)

data_selection$Date <- as.Date(data_selection$Date, format = "%d.%m.%Y")

#Analysis

require(forecast)

ma <- ma(data_selection[,2], 30)

model <- auto.arima(data_selection[,2])

forecast_period <- 90

model_forecast <- round(median(forecast(model, h = forecast_period)$mean))

png(sprintf("drg_model_%4s.png", Sys.Date()), width = 1920, height = 1080)

plot(forecast(model, h = forecast_period), ylab = "Dwarves mining on Hoxxes IV", xlab = sprintf("Days since DRG launch on 28.02.2018 (Model date:%4s)", Sys.Date()), lwd = 1)
lines(ma, col = "red", lwd = 5)
abline(v = c(365,365*2), col = "forestgreen", lty = 2, lwd = 0.5)
abline(h = c(5000, 10000, 15000), col = "grey", lty = 2, lwd = 0.5)
legend(0, 20000, legend=c("Data from SteamDB", "Forecast from ARIMA", "Monthly moving average", "One year elapsed"), col=c("black", "blue", "red", "forestgreen"), lty=c(1, 1, 1, 2), cex=2)
text(800, 250, "Rock and Stone", cex=1)
text(800, 12500, sprintf("Average estimated dwarves:%d", model_forecast), col = "blue", cex=1)

dev.off()
