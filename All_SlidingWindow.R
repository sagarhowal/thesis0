#This file is for reproducing all the sliding window methods forecasts for the Research Project 
#FORECASTING

#Set Working Directory Before Executing
#setwd("")

#Load all libraries
library(data.table)
library(rpart) 
library(rpart.plot) 
library(party) 
library(forecast) 
library(grid) 
library(lubridate)
library(rpart)
library(randomForest)
library(reshape2)
library(mgcv)


#Load all files
DT_agg <- read_feather("DT_agg.feather")
DT_ens <- read_feather("DT_ens.feather")
DT_gam <- read_feather("DT_gam.feather")
DT_wide_50 <- read_feather("DT_wide_50.feather")


#Define MAPE

mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}





################## Multiple Linear Regression 

#Create a function for predicting next week using last two weeks training data
predWeekReg <- function(data, set_of_date){
  
  # Subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- N / period # number of days in the train set
  # 1, ..., period, 1, ..., period - and so on for the daily season 
  # Using feature "week_num" for the weekly season
  matrix_train <- data.table(Load = data_train[, VALUE],
                             Daily = as.factor(rep(1:period, window)),
                             Weekly = as.factor(data_train[, week_num]))
  
  # Creation of the model
  lm_m <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train)
  
  # Creation of the forecast for one week ahead
  pred_week <- predict(lm_m, matrix_train[1:(7*period), -1, with = FALSE])
  
  return(as.vector(pred_week))
}


#Forecasting the whole year with 2 weeks training and 1 week prediction. 
n_weeks <- floor(length(n_date)/7) - 2

# Forecasts
lm_pred_weeks_1 <- sapply(0:(n_weeks-1), function(i)
  predWeekReg(DT_agg[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)]))


# Evaluation (computation of errors)
lm_err_mape_1 <- sapply(0:(n_weeks-1), function(i)
  mape(DT_agg[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), VALUE],
       lm_pred_weeks_1[, i+1]))


#Plot Distribution of MAPE
plot(density(lm_err_mape_1))

summary(lm_err_mape_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.884   2.482   3.518   4.537   5.158  12.810



################# Generalized Additive Models

predWeekGAM <- function(data, set_of_date){
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  # creation of training dataset
  N <- nrow(data_train)
  window <- N / period # number of days in the train set
  matrix_train <- data.table(Load = data_train[, value],
                             Daily = rep(1:period, window),
                             Weekly = data_train[, week_num])
  
  # model fit
  gam_m <- gam(Load ~ t2(Daily, Weekly,
                         k = c(period, 7),
                         bs = c("cr", "ps"),
                         full = TRUE),
               data = matrix_train,
               family = gaussian)
  
  # new data and prediction
  new.data <- matrix_train[1:(48*7), .(Daily, Weekly)]
  pred_week <- predict(gam_m, newdata = new.data, type = "response", se.fit = FALSE)
  
  return(list(GAMobj = gam_m, Pred = as.vector(pred_week)))
}


n_weeks <- floor(length(n_date)/7) - 2

gam_mape <- numeric()

for(i in 0:(n_weeks-1)){
  
  gam_pred_weeks <- predWeekGAM(DT_gam[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)])
  
  gam_err_mape <- mape(DT_gam[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
                       gam_pred_weeks$Pred)
  gam_mape <- union(gam_mape, c(gam_err_mape))
}

summary(gam_mape)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.231   2.821   5.291   5.768   7.576  11.489 




#################### Decision Trees and Ensemble Learning Models

# store information of the type of consumer, date, weekday and period
n_date <- unique(DT_ens[, date])
period <- 48


# split data on train and test part, pick aggregated Light Industrial consumers as example case.
data_train <- DT_ens[date %in% n_date[1:21]]
data_test <- DT_ens[date %in% n_date[22]]


# look on previous post on simple regression trees methods and forecasting
# trend, lag and fourier!
RpartTrend <- function(data, set_of_date, K, period = 48){
  
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_1 <- rpart(Load ~ ., data = matrix_train,
                  control = rpart.control(minsplit = 2,
                                          maxdepth = 30,
                                          cp = 0.000001))
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  # new data and prediction
  pred_tree <- predict(tree_1, matrix_test) + trend_for
  
  return(as.vector(pred_tree))
}

CtreeTrend <- function(data, set_of_date, K, period = 48){
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- party::ctree(Load ~ ., data = matrix_train,
                         controls = party::ctree_control(teststat = c("quad"),
                                                         testtype = c("Teststatistic"),
                                                         mincriterion = 0.925,
                                                         minsplit = 1,
                                                         minbucket = 1))
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + trend_for
  
  return(as.vector(pred_tree))
}


data_ts <- ts(data_train$value, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

trend_part <- ts(decomp_ts[,2])

trend_fit <- auto.arima(trend_part)
trend_for <- as.vector(forecast(trend_fit, period)$mean)

data_msts <- msts(data_train$value, seasonal.periods = c(period, period*7))

K <- 2
fuur <- fourier(data_msts, K = c(K, K)) # Fourier features to model (Daily and Weekly)

N <- nrow(data_train)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrending original time series
lag_seas <- decomp_ts[1:(period*window), 1] # lag feature to model

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)

N_boot <- 100 # number of bootstraps

pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- rpart(Load ~ ., data = matrixSam,
                    control = rpart.control(minsplit = sample(2:3, 1),
                                            maxdepth = sample(26:30, 1),
                                            cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}



# Do the same with CTREE

pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- party::ctree(Load ~ ., data = matrixSam,
                           controls = party::ctree_control(teststat = c("quad"),
                                                           testtype = c("Teststatistic"),
                                                           mincriterion = sample(seq(0.88, 0.97, by = 0.005), 1),
                                                           minsplit = 1,
                                                           minbucket = 1,
                                                           mtry = 0, maxdepth = 0))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}



# Grid Search ----
RFgrid <- function(data_train, param1, param2, K, period = 48) {
  
  N <- length(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.frame(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- randomForest(Load ~ ., data = matrix_train,
                         ntree = 1000, mtry = param1, nodesize = param2, importance = TRUE)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}

mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real))) # MAPE - Mean Absolute Percentage Error
}

gridSearch <- function(Y, train.win = 21, FUN, param1, param2, period = 48) {
  
  days <- length(Y)/period
  test.days <- days - train.win
  mape.matrix <- matrix(0, nrow = length(param1), ncol = length(param2))
  row.names(mape.matrix) <- param1
  colnames(mape.matrix) <- param2
  forecast.svr <- vector(length = test.days*period)
  
  for(i in seq_along(param1)){
    for(j in seq_along(param2)){
      for(k in 0:(test.days-1)){
        train.set <- Y[((k*period)+1):((period*k)+(train.win*period))]
        forecast.svr[((k*period)+1):((k+1)*period)] <- FUN(train.set, param1 = param1[i], param2 = param2[j], K = 2)
      }
      mape.matrix[i,j] <- mape(Y[-(1:(train.win*period))], forecast.svr)
    }
  }
  return(mape.matrix)
}

all_data <- DT_ens$value[1:(period*41)]
res_1 <- gridSearch(all_data, FUN = RFgrid, param1 = c(2,3,4,5,6), param2 = c(2,3,4,5,6))

res_1

c(mtry = row.names(res_1)[which(res_1 == min(res_1), arr.ind = TRUE)[1]],
  nodesize = colnames(res_1)[which(res_1 == min(res_1), arr.ind = TRUE)[2]])

data_grid <- data.table(melt(res_1))
colnames(data_grid) <- c("mtry", "nodesize", "MAPE")

ggplot(data_grid, aes(mtry, nodesize, size = MAPE, color = MAPE)) +
  geom_point() +
  scale_color_distiller(palette = "Reds") +
  theme_ts

# Comparison ----
RFTrend <- function(data, set_of_date, K, period = 48) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.frame(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- randomForest(Load ~ ., data = matrix_train,
                         ntree = 1000, mtry = 2, nodesize = 3, importance = TRUE)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}

RpartBaggTrend <- function(data, set_of_date, K, period = 48, N_boot = 100) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_mat <- matrix(0, nrow = N_boot, ncol = period)
  for(i in 1:N_boot) {
    matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
    tree_bag <- rpart(Load ~ ., data = matrixSam,
                      control = rpart.control(minsplit = sample(2:3, 1),
                                              maxdepth = sample(26:30, 1),
                                              cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
    
    # new data and prediction
    pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
  }
  
  return(as.vector(apply(pred_mat, 2, median)))
}

CtreeBaggTrend <- function(data, set_of_date, K, period = 48, N_boot = 100) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_mat <- matrix(0, nrow = N_boot, ncol = period)
  for(i in 1:N_boot) {
    matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
    tree_bag <- party::ctree(Load ~ ., data = matrixSam,
                             controls = party::ctree_control(teststat = c("quad"),
                                                             testtype = c("Teststatistic"),
                                                             mincriterion = sample(seq(0.88, 0.97, by = 0.005), 1),
                                                             minsplit = 1,
                                                             minbucket = 1,
                                                             mtry = 0, maxdepth = 0))
    
    # new data and prediction
    pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
  }
  
  return(as.vector(apply(pred_mat, 2, median)))
}



# Evaluation -- Run for All models --
n_days <- floor(length(n_date)) - 21

for_rpart <- sapply(0:(n_days-1), function(i)
  RpartBaggTrend(DT_ens, n_date[(i+1):(i+7*3)], K = 2))

for_ctree <- sapply(0:(n_days-1), function(i)
  CtreeBaggTrend(DT_ens, n_date[(i+1):(i+7*3)], K = 2))

for_rpart_sim <- sapply(0:(n_days-1), function(i)
  RpartTrend(DT_ens, n_date[(i+1):(i+7*3)], K = 2))

for_ctree_sim <- sapply(0:(n_days-1), function(i)
  CtreeTrend(DT_ens, n_date[(i+1):(i+7*3)], K = 2))

for_rf <- sapply(0:(n_days-1), function(i)
  RFTrend(DT_ens, n_date[(i+1):(i+7*3)], K = 2))

err_mape_rpart <- sapply(0:(n_days-1), function(i)
  mape(DT_ens[date %in% n_date[22+i], value],
       for_rpart[,i+1]))

err_mape_ctree <- sapply(0:(n_days-1), function(i)
  mape(DT_ens[date %in% n_date[22+i], value],
       for_ctree[,i+1]))

err_mape_rpart_sim <- sapply(0:(n_days-1), function(i)
  mape(DT_ens[date %in% n_date[22+i], value],
       for_rpart_sim[,i+1]))

err_mape_ctree_sim <- sapply(0:(n_days-1), function(i)
  mape(DT_ens[date %in% n_date[22+i], value],
       for_ctree_sim[,i+1]))

err_mape_rf <- sapply(0:(n_days-1), function(i)
  mape(DT_ens[date %in% n_date[22+i], value],
       for_rf[,i+1]))

summary(err_mape_rpart)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.418   1.988   2.551   2.966   3.111  10.892
summary(err_mape_ctree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.228   2.176   2.584   2.998   3.149   9.055
summary(err_mape_rpart_sim)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.538   2.098   2.648   3.107   3.225  11.330 
summary(err_mape_ctree_sim)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.328   2.294   2.721   3.108   3.383   9.188
summary(err_mape_rf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.199   1.995   2.478   2.872   3.039   9.849














