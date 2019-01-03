setwd("/Users/sagarhowal/Code/Thesis/CER Electricity Revised March 2012")

library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)


#Load Data
load("RnD_Clean.RData")

#cleanup if needed [after loading]
rm(DT1491, DT1491New, DT1951, DT1951New, DT1951New2, NArows, rez_full)
rm(corrected_date, DateOrig, rezidences, ts3)

#-----------------New--------------------------------
DT_agg <- as.data.table(aggregate(DT[, .(VALUE)], by = DT[, .(TIME)],
                                  FUN = sum, simplify = TRUE))

DT_agg$DATETIME <- rep(ts)

DT_agg <- subset(DT_agg, DT_agg$DATETIME >= "2010-01-01 00:00:00" & DT_agg$DATETIME <= "2010-12-31 23:30:00" )


ggplot(data = DT_agg, aes(x = DATETIME, y = VALUE)) +
  geom_line() + 
  #facet_grid(type ~ ., scales = "free_y") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 9, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")



#Analysis
DT_agg[, week:= weekdays(DATETIME)]
DT_agg[, week_num := as.integer(as.factor(DT_agg[, week]))]
DT_agg[, type := rep("Residential")]
DT_agg[, date := date(DATETIME)]

n_type <- unique(DT_agg[, type])
n_date <- unique(DT_agg[, date])
n_weekdays <- unique(DT_agg[, week])
period <- 48



data_r <- DT_agg[(type == n_type[1] & date %in% n_date[57:70])]

ggplot(data_r, aes(DATETIME, VALUE)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")


N <- nrow(data_r)
window <- N / period # number of days in the train set
# 1, ..., period, 1, ..., period - and so on for the daily season 
# using feature "week_num" for the weekly season
matrix_train <- data.table(Load = data_r[, VALUE],
                           Daily = as.factor(rep(1:period, window)),
                           Weekly = as.factor(data_r[, week_num]))


#Model [Linear Model]
lm_m_1 <- lm(Load ~ 0 + ., data = matrix_train)

smmr_1 <- summary(lm_m_1)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))


datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = lm_m_1$fitted.values, data_time = data_r[, DATETIME])))
datas[, type := rep(c("Real", "Fitted"), each = nrow(data_r))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from MLR")

#Plot the Residuals
ggplot(data = data.table(Fitted_values = lm_m_1$fitted.values,
                         Residuals = lm_m_1$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")


#QQ-Plot
ggQQ <- function(lm){
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(shape = 1, size = 3) +         # open circles
    labs(title = "Normal Q-Q",             # plot title
         x = "Theoretical Quantiles",      # x-axis label
         y = "Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype = "dashed",
                size = 1, col = "firebrick1") # dashed reference line
  return(p)
}

#Plot
ggQQ(lm_m_1) #Not a good fit. 


#Model 2 [with interactions]
lm_m_2 <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly,
             data = matrix_train)

c(Previous = summary(lm_m_1)$r.squared, New = summary(lm_m_2)$r.squared)
# Previous       New 
# 0.9973367 0.9995551 
#Naaaaaayce! 

datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = lm_m_2$fitted.values, data_time = data_r[, DATETIME])))
datas[, type := rep(c("Real", "Fitted"), each = nrow(data_r))]

#Plot Fitted vs Real for Model 2
ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from MLR")

#Plot Resuduals for Model 2
ggplot(data = data.table(Fitted_values = lm_m_2$fitted.values,
                         Residuals = lm_m_2$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")

#QQ-Plot for Model 2
ggQQ(lm_m_2)



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

#Define MAPE
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
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



#Save Preprocessed Dataframe for fast execution Later
write_feather(DT_agg, "DT_agg.feather")












