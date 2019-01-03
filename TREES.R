library(data.table)
library(rpart) 
library(rpart.plot) 
library(party) 
library(forecast) 
library(ggplot2) 
library(ggforce) 
library(plotly) #doesn't work for some reason. Maybe Version compatability issue
library(grid) 


DT_T <- subset(DT_agg, DT_agg$DATETIME >= "2010-05-02 00:00:00" & DT_agg$DATETIME <= "2010-08-28 23:30:00" )

library(lubridate)
DT_T[, week:= weekdays(as.POSIXct(DATETIME))]
DT_T[, week_num := as.integer(as.factor(DT_T[, week]))]


#Begin
n_date <- unique(DT_T[, date])
period <- 48


#Set Theme to Visualize the data
library(ggplot2)
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

#Test Train Split
data_train <- DT_T[date %in% n_date[43:63]]
data_test <- DT_T[date %in% n_date[64]]


#Compute and Plot weekly Averages
averages <- data.table(VALUE = rep(sapply(0:2, function(i)
  mean(data_train[((i*period*7)+1):((i+1)*period*7), VALUE])),
  each = period * 7),
  DATETIME = data_train$DATETIME)
#Plotting now
ggplot(data_train, aes(DATETIME, VALUE)) +
  geom_line() +
  geom_line(data = averages, aes(DATETIME, VALUE),
            linetype = 5, alpha = 0.75, size = 1.2, color = "firebrick2") +
  labs(x = "Date", y = "Load (kW)") +
  theme_ts


#We can see the trend growing changing. Performing STL
data_ts <- ts(data_train$VALUE, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

decomp_stl <- data.table(Load = c(data_train$VALUE, as.numeric(decomp_ts)),
                         Date = rep(data_train[,DATETIME], ncol(decomp_ts)+1),
                         Type = factor(rep(c("original data", colnames(decomp_ts)),
                                           each = nrow(decomp_ts)),
                                       levels = c("original data", colnames(decomp_ts))))

ggplot(decomp_stl, aes(x = Date, y = Load)) +
  geom_line() + 
  facet_grid(Type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Time Series Decomposition by STL") +
  theme_ts





#Feature Engineering
library(forecast)
data_msts <- msts(data_train$VALUE, seasonal.periods = c(period, period*7))

#Fourier Transform 
K <- 2
fuur <- fourier(data_msts, K = c(K, K))

#Visualize the Fourier Series
Daily <- rep(1:period, 21) # simple daily vector
Weekly <- data_train[, week_num] # simple weekly vector

data_fuur_simple <- data.table(VALUE = c(scale(Daily), fuur[,2], scale(Weekly), fuur[,6]),
                               date = rep(data_train$DATETIME, 4),
                               method = rep(c("simple-daily", "four-daily",
                                              "simple-weekly", "four-weekly"),
                                            each = nrow(fuur)),
                               type = rep(c("Daily season", "Weekly season"),
                                          each = nrow(fuur)*2))
#plot them. 
ggplot(data_fuur_simple, aes(x = date, y = VALUE, color = method)) +
  geom_line(size = 1.2, alpha = 0.7) + 
  facet_grid(type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Features Comparison") +
  theme_ts



#forecast Decomp of Autoarima
library(forecast)
trend_part <- ts(decomp_ts[,2])
trend_fit <- auto.arima(trend_part)
summary(trend_fit)
                    # Series: trend_part 
                    # ARIMA(0,2,0) 
trend_for <- forecast(trend_fit, period)$mean

#PLOT
#Create Trend Dataframe for plotting
trend_data <- data.table(Load = c(decomp_ts[,2], trend_for),
                         Date = c(data_train$DATETIME, data_test$DATETIME),
                         Type = c(rep("Real", nrow(data_train)), rep("Forecast",
                                                                     nrow(data_test))))
#Plot
ggplot(trend_data, aes(Date, Load, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = paste(trend_fit)) +
  theme_ts

#Since auto-arima chose ARIMA (0,2,0)... Create features for it. 
N <- nrow(data_train)
window <- (N / period) - 1 # number of days in train set minus lag

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrended load
lag_seas <- decomp_ts[1:(period*window), 1] # seasonal part of time series as lag feature

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

#Define MAPE
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real))) # MAPE - Mean Absolute Percentage Error
}



#RPART CART TREE MODELLING Begins
library(rpart) 
library(rpart.plot) 
library(party) 

#1. Default Setting
tree_1 <- rpart(Load ~ ., data = matrix_train)

#check importances
tree_1$variable.importance
#       Lag     S1-48     C1-48     S2-48    S1-336     C2-48    C1-336    S2-336 
# 579706969 193855846 154411515  29749820  26294109  17691461  15735774   7102806 

#check number of splits
paste("Number of splits: ", tree_1$cptable[dim(tree_1$cptable)[1], "nsplit"])
# [1] "Number of splits:  4"

#Plot tree_1
rpart.plot(tree_1, digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")


#Plot Fitted Values
datas <- data.table(Load = c(matrix_train$Load,
                             predict(tree_1)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "RPART"), each = length(matrix_train$Load)))

ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from RPART tree") +
  theme_ts

#Check MAPE 
mape(matrix_train$Load, predict(tree_1))
# Wow! 49.31821 
#Horrible

#Tree 2. Control the rpart. Increase Complexity "cp"
tree_2 <- rpart(Load ~ ., data = matrix_train,
                control = rpart.control(minsplit = 2,
                                        maxdepth = 30,
                                        cp = 0.000001))

#Plot the splits
plot(tree_2, compress = TRUE)

#check Splits
tree_2$cptable[dim(tree_2$cptable)[1], "nsplit"] # Number of splits
# [1] 480 (obviously)


#Again Check fitted values
#And Plot them
datas <- data.table(Load = c(matrix_train$Load,
                             predict(tree_2)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "RPART"), each = length(matrix_train$Load)))

ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from RPART") +
  theme_ts
#Wow Looks good! 

#Check MAPE
mape(matrix_train$Load, predict(tree_2))
# [1] 6.0766 ekach number na bhau


#Compute all above for Test as well
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)



# Forecast tree_2 with trend which we subtracted before. 
for_rpart <- predict(tree_2, matrix_test) + trend_for


data_for <- data.table(Load = c(data_train$VALUE, data_test$VALUE, for_rpart),
                       Date = c(data_train$DATETIME, rep(data_test$DATETIME, 2)),
                       Type = c(rep("Train data", nrow(data_train)),
                                rep("Test data", nrow(data_test)),
                                rep("Forecast", nrow(data_test))))


library(ggforce) #for facet_zoom
ggplot(data_for, aes(Date, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  facet_zoom(x = Date %in% data_test$DATETIME, zoom.size = 1.2) +
  labs(title = "Forecast from RPART") +
  theme_ts

#-------------------------- OPTIONAL ---------(Code works)-------------------------------
#Plot without using the trend 
matrix_train_sim <- data.table(Load = tail(data_train$VALUE, window*period),
                               fuur[(period+1):N,],
                               Lag = lag_seas)

tree_sim <- rpart(Load ~ ., data = matrix_train_sim,
                  control = rpart.control(minsplit = 2,
                                          maxdepth = 30,
                                          cp = 0.000001))

for_rpart_sim <- predict(tree_sim, matrix_test)

data_for <- data.table(Load = c(data_train$VALUE, data_test$VALUE, for_rpart, for_rpart_sim),
                       Date = c(data_train$DATETIME, rep(data_test$DATETIME, 3)),
                       Type = c(rep("Train data", nrow(data_train)),
                                rep("Test data", nrow(data_test)),
                                rep("Forecast with trend", nrow(data_test)),
                                rep("Forecast simple", nrow(data_test))))

ggplot(data_for, aes(Date, Load, color = Type, linetype = Type)) +
  geom_line(size = 0.8, alpha = 0.7) +
  facet_zoom(x = Date %in% data_test$date_time, zoom.size = 1.2) +
  labs(title = "Forecasts from RPARTs with and without trend forecasting") +
  scale_linetype_manual(values = c(5,6,1,1)) +
  theme_ts


# This technique gives high value of MAPE since there is a misfit between the
# real and predicted due to Trend not being included.
#----------------------------------------------------------------------------

# -------- CTREE ----------

#Run Model with Default parameters
ctree_1 <- ctree(Load ~ ., data = matrix_train)

datas <- data.table(Load = c(matrix_train$Load,
                             predict(ctree_1)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "CTREE"), each = length(matrix_train$Load)))

ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from CTREE") +
  theme_ts

#Get MAPE for this 
mape(matrix_train$Load, predict(ctree_1))
# [1] 25.45121 very high. 

#MODEL 2

#mincriterion should be > (1 - p-value). The minsplit and minbucket is for complexity. 
ctree_2 <- ctree(Load ~ ., data = matrix_train,
                 controls = party::ctree_control(teststat = "quad", 
                                                 testtype = "Teststatistic", 
                                                 mincriterion = 0.925,
                                                 minsplit = 1,
                                                 minbucket = 1))

datas <- data.table(Load = c(matrix_train$Load,
                             predict(ctree_2)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "CTREE"), each = length(matrix_train$Load)))

ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from CTREE") +
  theme_ts


# Get MAPE
mape(matrix_train$Load, predict(ctree_2))
# [1] 15.35575 ---Better


# Predict with model with trend
for_ctree <- predict(ctree_2, matrix_test) + trend_for

# Compare Both Models 
data_for <- data.table(Load = c(data_train$VALUE, data_test$VALUE, for_rpart, for_ctree),
                       Date = c(data_train$DATETIME, rep(data_test$DATETIME, 3)),
                       Type = c(rep("Train data", nrow(data_train)),
                                rep("Test data", nrow(data_test)),
                                rep("RPART", nrow(data_test)),
                                rep("CTREE", nrow(data_test))))

#Plot 
ggplot(data_for, aes(Date, Load, color = Type, linetype = Type)) +
  geom_line(size = 0.8, alpha = 0.7) +
  facet_zoom(x = Date %in% data_test$DATETIME, zoom.size = 1.2) +
  labs(title = "Forecasts from RPART and CTREE models") +
  scale_linetype_manual(values = c(5,6,1,1)) +
  theme_ts


#Get MAPEs for them 
mape(data_test$VALUE, for_rpart)
# [1] 1.839976
mape(data_test$VALUE, for_ctree)
# [1] 2.362905
# RPart with Trend is better than CTree with trend 
# Naaaaayce 



#Remember to add the combined modelling function for sliding window approach in this file 

# Sliding Window Approach for RPART and CTREE

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
  
  # prediction
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
                         controls = party::ctree_control(teststat = "quad",
                                                         testtype = "Teststatistic",
                                                         mincriterion = 0.925,
                                                         minsplit = 1,
                                                         minbucket = 1))
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + trend_for
  
  return(as.vector(pred_tree))
}








