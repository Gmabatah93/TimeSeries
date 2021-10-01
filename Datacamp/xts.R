library(readr)
library(dplyr)
library(xts)

stocks <- read_delim("https://assets.datacamp.com/production/repositories/693/datasets/ef3ee852cd87957da3cb4e65ee435c6e2f718966/dataset_1_1.csv")
stocks_xts <- xts(x = as.ts(stocks), order.by = stocks$Index)
stocks_xts$Index <- NULL

index(stocks_xts)
coredata(stocks_xts)

plot(stocks_xts$microsoft)
