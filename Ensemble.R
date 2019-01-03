# Using the same dataset we used for the Decision trees algorithms. 
# DT_T, that is


# Begin similarly 
n_date <- unique(DT_T[, date])
period <- 48


# Set Theme for ggplot2
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))



# Using 3 weeks data to forecast next day
data_train <- DT_T[date %in% n_date[1:21]]
data_test <- DT_T[date %in% n_date[22]]

# Visualize the Train set
ggplot(data_train, aes(DATETIME, VALUE)) +
  geom_line() +
  labs(x = "Date", y = "Load (kW)") +
  theme_ts


# Modelling


# RPART with Bagging
library(forecast)
data_ts <- ts(data_train$VALUE, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

trend_part <- ts(decomp_ts[,2])

trend_fit <- auto.arima(trend_part) # ARIMA
trend_for <- as.vector(forecast(trend_fit, period)$mean) # trend forecast

data_msts <- msts(data_train$VALUE, seasonal.periods = c(period, period*7))

K <- 2
fuur <- fourier(data_msts, K = c(K, K)) # Fourier features to model (Daily and Weekly)

N <- nrow(data_train)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrended original time series
lag_seas <- decomp_ts[1:(period*window), 1] # lag feature to model

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)

library(rpart)
# number of bootstraps = 100 
N_boot <- 100 # number of bootstraps

#Start modelling
pred_mat <- matrix(0, nrow = N_boot, ncol = period)

# Training loop for 100 bootstraps
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period),
                                   floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)),
                                   replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- rpart(Load ~ ., data = matrixSam,
                    control = rpart.control(minsplit = sample(2:3, 1),
                                            maxdepth = sample(26:30, 1),
                                            cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}


#Predict
pred_melt_rpart <- data.table(melt(pred_mat))

pred_ave_rpart <- pred_melt_rpart[, .(value = median(value)), by = .(Var2)]
pred_ave_rpart[, Var1 := "RPART_Bagg"]

ggplot(pred_melt_rpart, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_rpart, aes(Var2, value), color = "firebrick2", alpha = 0.9, size = 2) +
  labs(x = "Time", y = "Load (kW)", title = "Bagging with RPART") +
  theme_ts



# # Load the previous sliding window model Functions before this! 
# 
# #Using RPART
# simple_rpart <- RpartTrend(DT_T, n_date[1:21], K = 2)
# pred_rpart <- data.table(VALUE = simple_rpart, Var2 = 1:48, Var1 = "RPART")
# 
# ggplot(pred_melt_rpart, aes(Var2, value, group = Var1)) +
#   geom_line(alpha = 0.75) +
#   geom_line(data = pred_ave_rpart, aes(Var2, value), color = "firebrick2", alpha = 0.8, size = 1.8) +
#   geom_line(data = pred_rpart, aes(Var2, value), color = "dodgerblue2", alpha = 0.8, size = 1.8) +
#   labs(x = "Time", y = "Load (kW)", title = "Bagging with RPART and comparison with simple RPART") +
#   theme_ts


# RANDOM FOREST
library(randomForest)
rf_model <- randomForest(Load ~ ., data = data.frame(matrix_train),
                         ntree = 1000, mtry = 3, nodesize = 5, importance = TRUE)

#Variable Importance
varImpPlot(rf_model, main = "Variable importance")

#Predict
pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for) #Make sure to add Arima of trend

pred_rf <- data.table(value = pred_rf, Var2 = 1:48, Var1 = "RF") 

pred_true <- data.table(value = data_test$VALUE, Var2 = 1:48, Var1 = "Real")
preds_all <- rbindlist(list(pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Time", y = "Load (kW)", title = "Random Forest + Trend ARIMA - Prediction vs. Real") +
  guides(color=guide_legend(title="Method")) +
  theme_ts

#
#
# RFTrend Function
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
                         ntree = 1000, mtry = 6, nodesize = 2, importance = TRUE)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}








# SLIDING WINDOW For RANDOM FOREST
#
#
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

#Grid Search
gridSearch <- function(Y, train.win = 21, FUN, param1, param2, period = 48) {
  
  days <- length(Y)/period
  test.days <- days - train.win
  mape.matrix <- matrix(0, nrow = length(param1), ncol = length(param2))
  row.names(mape.matrix) <- param1
  colnames(mape.matrix) <- param2
  forecast.rf <- vector(length = test.days*period)
  
  for(i in seq_along(param1)){
    for(j in seq_along(param2)){
      for(k in 0:(test.days-1)){
        train.set <- Y[((k*period)+1):((period*k)+(train.win*period))]
        forecast.rf[((k*period)+1):((k+1)*period)] <- FUN(train.set, param1 = param1[i], param2 = param2[j], K = 2)
      }
      mape.matrix[i,j] <- mape(Y[-(1:(train.win*period))], forecast.rf)
    }
  }
  return(mape.matrix)
}

DT_ens <- DT_T
names(DT_ens)[names(DT_ens) == "VALUE"] = "value"
names(DT_ens)[names(DT_ens) == "DATETIME"] = "date_time"

write_feather(DT_ens, "DT_ens.feather")




















