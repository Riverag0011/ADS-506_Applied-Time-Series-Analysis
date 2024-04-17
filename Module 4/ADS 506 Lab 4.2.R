library(tidyverse)
library(fpp2)

set.seed(506)

er_arrivals <- read_csv("Data/ER Arrivals.csv", 
                        col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
View(er_arrivals)



arrivals <- ts(er_arrivals$Arrivals, frequency = 24)
arrivals

# let's plot this
autoplot(arrivals) +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals")


mean_arr <- mean(er_arrivals$Arrivals)

# plot the mean
autoplot(arrivals) +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals") +
  geom_hline(yintercept = mean_arr, color = 'green', size = 1) +
  theme_classic()


train <- window(arrivals, end = c(16, 24))
test <- window(arrivals, start = c(17,1))


# models
mean24 <- meanf(train, h = 24)
naive24 <- rwf(train, h = 24)
snaive24 <- snaive(train, h = 24)
helpme <- forecast(train, h = 24)


autoplot(train) + 
  autolayer(helpme, color = "green", PI = F) +
  autolayer(test, color = "red") +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals") +
  coord_cartesian(xlim = c(15, 18))



# test

accuracy(mean24, test)
accuracy(naive24, test)
accuracy(snaive24, test)
accuracy(helpme, test)





