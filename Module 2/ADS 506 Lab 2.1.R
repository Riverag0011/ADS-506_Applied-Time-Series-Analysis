library(fpp2)
library(zoo)
library(readr)

set.seed(506)

AustralianWines <- read_csv("AustralianWines.csv")

reds <- ts(AustralianWines$Red, start = c(1980, 1), frequency = 12)

autoplot(reds)

acf(reds, lag.max = 48)



# moving average

?rollmean

reds.ma <- rollmean(reds, k = 4, align = "right")

autoplot(reds, series = "Actual") +
  autolayer(reds.ma, series = "MovingAvg") +
  theme_classic()


library(quantmod)
getSymbols("AAPl", from = "2015-1-1", to = "2015-12-31")

AAPL

apple <- ts(AAPL$AAPL.Close)

apple.train <- window(apple, end = 241)
apple.test <- window(apple, start = 242)

length(apple.test)

apple.model <- ses(apple.train, alpha = .8, level = c(.95))
apple.pred <- forecast(apple.model, h = 10)

autoplot(apple, series = "actual") +
  autolayer(apple.pred, series = "predicted", alpha = .4) +
  theme_classic() +
  coord_cartesian(xlim = c(210, 251))


autoplot(apple.train, series = "Training") +
  autolayer(apple.model$fitted, series = "Model") +
  theme_classic() +
  coord_cartesian(xlim = c(201, 241))


# choose alpha with the lowest RMSE

summary(apple.model)

summary( ses(apple.train, alpha = .9, level = c(.95)) )


#.8 rmse = 0.4996302 
#.5 rmse = 0.5457429 
#.9 rmse = 0.4947192 

(0.4996302 - 0.5457429)/0.5457429
















