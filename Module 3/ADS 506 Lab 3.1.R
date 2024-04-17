library(fpp2)
library(zoo)
library(readr)
library(dplyr)

set.seed(506)

SeoulBikeData <- read_csv("SeoulBikeData.csv", 
                          col_types = cols(Date = col_date(format = "%d|%m|%Y")))

head(SeoulBikeData)


bikes <- ts(SeoulBikeData$BikesRented, frequency = 7)




autoplot(bikes)

acf(bikes)

# take a first order difference

# to determine the ar()
acf(diff(bikes))

# to determine the ma()
pacf(diff(bikes))


# our arima model
my_arima <- arima(bikes, order = c(1, 1, 3))
summary(my_arima)


auto_model <- auto.arima(bikes)
summary(auto_model)


# yt = b0 + b1xt + b2x2t + b3x3t

# yt = b0 + b1yt-2 + b2yt-2+ b3yt-3+ ....

# our model 
# yt = b0 + b1yt-1 + Oeyt-1 + Oeyt-2 + Oeyt-3


# what about extermal predictors
head(SeoulBikeData)

training_set <- SeoulBikeData[1:335, ] %>%
  select(-Date)

test_set <- SeoulBikeData[336:365, ] %>%
  select(-Date)


outcome_v <- training_set$BikesRented
predictors <- as.matrix(training_set[, 1:4])

# auto arima function
auto_arima_reg_model <- auto.arima(outcome_v, xreg = predictors)
summary(auto_arima_reg_model)

sqrt(mean(auto_arima_reg_model$residuals^2))


my_arima_reg_model <- Arima(outcome_v, order = c(1,1,3), xreg = predictors)
summary(my_arima_reg_model)




# forecastinng

my_predictors <- as.matrix(test_set[, 1:4])

my_forecast <- forecast(auto_arima_reg_model, xreg = my_predictors)

my_forecast

autoplot(ts(SeoulBikeData$BikesRented), color = 'red') +
  autolayer(my_forecast, alpha = .3) +
  coord_cartesian(xlim = c(300, 370))








































