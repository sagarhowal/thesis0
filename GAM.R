#setwd("/Users/sagarhowal/Code/Thesis/CER Electricity Revised March 2012")

#Starting with Data from the MLR.RData file. 
Dates2010 <- seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by="days")
Days2010 <- weekdays(Dates2010)

DateDay <- data.frame(date=as.Date(character()), 
                         weekdays=character())


DateDay <- do.call(rbind, Map(data.frame, date=Dates2010, day=Days2010))

DateDay <- as.data.frame(lapply(DateDay, rep, 48))
library(plyr)
DateDay<- arrange(DateDay, date)

DT_agg$Week <- NULL
DT_agg$week <- DateDay$day
DT_agg$date <- DateDay$date

library(car)
DT_agg[, week_num := as.integer(car::recode(week,
                  "'Monday'='1';'Tuesday'='2';'Wednesday'='3';'Thursday'='4';
    'Friday'='5';'Saturday'='6';'Sunday'='7'"))]


n_type <- unique(DT_agg[, type])
n_date <- unique(DT_agg[, date])
n_weekdays <- unique(DT_agg[, week])
period <- 48



data_r <- DT_agg[(type == n_type[1] & date %in% n_date[57:70])]

library(ggplot2)
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



N <- nrow(data_r) # number of observations in the train set
window <- N / period # number of days in the train set
matrix_gam <- data.table(Load = data_r[, VALUE],
                         Daily = rep(1:period, window),
                         Weekly = data_r[, week_num])


gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7),
             data = matrix_gam,
             family = gaussian)

gam_2 <- GAMBoost(matrix_gam$Load, s(matrix_gam$Daily, bs = "cr", k = period) +
               s(matrix_gam$Weekly, bs = "ps", k = 7))

#Create GAM Model
library(mgcv)
gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7),
             data = matrix_gam,
             family = gaussian)

#Plot GAM
layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)

#check Model
summary(gam_1)
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Load ~ s(Daily, bs = "cr", k = period) + s(Weekly, bs = "ps", 
#                                              k = 7)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3409.26       7.39   461.3   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#               edf   Ref.df      F p-value    
# s(Daily)  19.645  24.423  910.1   <2e-16 ***
# s(Weekly)  5.731    5.961  26.5   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# R-sq.(adj) =  0.971   Deviance explained = 97.2%
# GCV =  38199  Scale est. = 36700     n = 672


datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = gam_1$fitted.values,
                                   data_time = data_r[, DATETIME])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.1")


#What Optimizer are you using? 
gam_1$optimizer
# [1] "magic"


#GAM Model 2
gam_2 <- gam(Load ~ s(Daily, Weekly),
             data = matrix_gam,
             family = gaussian)

summary(gam_2)$r.sq
# [1] 0.9847138

summary(gam_2)$s.table
#                     edf   Ref.df    F             p-value
# s(Daily,Weekly) 28.52177 28.98531   1491.021       0


#Plot Model 2
datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = gam_2$fitted.values,
                                   DATETIME = data_r[, DATETIME])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.2")


# Model 3
gam_3 <- gam(Load ~ te(Daily, Weekly,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(gam_3)$r.sq
# [1] 0.8432316
#Horrible. very bad compared to model 1 and 2. 


summary(gam_3)$s.table

#                       edf  Ref.df       F p-value
# te(Daily,Weekly) 12.98248 14.5019 249.148       0

datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = gam_3$fitted.values,
                                   DATETIME = data_r[, DATETIME])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.3")
#Very Bad


# Model 4 
gam_4 <- gam(Load ~ te(Daily, Weekly,
                       k = c(period, 7),
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(gam_4)$r.sq
# [1] 0.9927867


summary(gam_4)$sp.criterion
#   GCV.Cp 
# 11294.48 

summary(gam_4)$s.table
#                     edf   Ref.df        F p-value
# te(Daily,Weekly) 129.657 164.8906 559.6096       0

#Plot Model 4
datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = gam_4$fitted.values,
                                   DATETIME = data_r[, DATETIME])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.4")


# Model 4 with fix
gam_4_fx <- gam(Load ~ te(Daily, Weekly,
                          k = c(period, 7),
                          bs = c("cr", "ps"),
                          fx = TRUE),
                data = matrix_gam,
                family = gaussian)

summary(gam_4_fx)$r.sq
# [1] 0.9909114


summary(gam_4_fx)$s.table
#                 edf Ref.df        F       p-value
# te(Daily,Weekly) 335    335 219.3804 3.698723e-295


#Try both Models 
gam_5 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7) +
               ti(Daily, Weekly,
                  k = c(period, 7),
                  bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(gam_5)$r.sq
# [1] 0.9932403


summary(gam_5)$sp.criterion
#   GCV.Cp 
# 10467.83 

summary(gam_5)$s.table
#                       edf     Ref.df         F       p-value
# s(Daily)         33.442231  39.771503 2409.3245  0.000000e+00
# s(Weekly)         5.976661   5.999694  115.3295 8.850551e-128
# ti(Daily,Weekly) 84.218723 113.852393   17.9932 9.931850e-297


gam_6 <- gam(Load ~ t2(Daily, Weekly,
                       k = c(period, 7),
                       bs = c("cr", "ps"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)

summary(gam_6)$r.sq
# [1] 0.9933721

summary(gam_6)$sp.criterion
#   GCV.Cp 
# 9964.745 


summary(gam_6)$s.table
#                       edf   Ref.df       F p-value
# t2(Daily,Weekly) 107.2137 130.6045 144.825       0


#Check AIC characteristic for these models
AIC(gam_1, gam_2, gam_3, gam_4, gam_5, gam_6)

# df       AIC
# gam_1  27.37664  8997.968
# gam_2  30.52177  8568.457
# gam_3  14.98248 10117.733
# gam_4 131.65698  8150.938
# gam_5 125.63762  8102.685
# gam_6 109.21373  8076.473
# * GAM_6 has the lowest AIC value


datas <- rbindlist(list(data_r[, .(VALUE, DATETIME)],
                        data.table(VALUE = gam_5$fitted.values,
                                   DATETIME = data_r[, DATETIME])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(DATETIME, VALUE, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM_6 Model")


gam.check(gam_4)
gam.check(gam_6)

layout(matrix(1:2, nrow = 1))
plot(gam_4, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.4 with te()")
plot(gam_5, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.5 with t2()")

#Visualize GAM4
vis.gam(gam_6, n.grid = 50, theta = 35, phi = 32, zlab = "",
        ticktype = "detailed", color = "topo", main = "Gam_6 Model - t2(Daily, Weekly)")


# ######
# #####-------Noel's Code------
# #Nols's Code
# densityPlot(DT_agg[DT_agg$week == "Saturday",]$VALUE)
# library(fitdistrplus)
# descdist(DT_agg$VALUE)
# densityPlot(log(DT_agg$VALUE))
# densityPlot(DT_agg$VALUE^(1/2))
# densityPlot(DT_agg$VALUE^(1/4))
# descdist(log(DT_agg$VALUE))
# 
# library(GAMBoost)
# gam_boost <- GAMBoost(matrix_gam$Load, s(matrix_gam$Daily, bs = "cr", k = period) +
#                     s(matrix_gam$Weekly, bs = "ps", k = 7))
# s(matrix_gam$Daily, matrix_gam$Daily + matrix_gam$Weekly)
# gam_boost_2 <- GAMBoost(matrix_gam$Load, matrix_gam$Daily)
# library(lmtest)
# bgtest(matrix_gam$Load)
# ###





#Autoregressive on the Residuals of the GAM models. 
gam_6_ar0 <- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
                  data = matrix_gam,
                  family = gaussian,
                  method = "REML")

gam_6_ar1 <- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
                  data = matrix_gam,
                  family = gaussian,
                  correlation = corARMA(form = ~ 1|Weekly, p = 1),
                  method = "REML")


#Anova on the Models 
anova(gam_6_ar0$lme, gam_6_ar1$lme)
#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# gam_6_ar0$lme     1 10 8333.865 8378.908 -4156.933                        
# gam_6_ar1$lme     2 11 7584.062 7633.609 -3781.031 1 vs 2 751.8037  <.0001



#Estimate ?? coefficient of AR(1) process
intervals(gam_6_ar1$lme, which = "var-cov")$corStruct
#         lower      est.     upper
# Phi 0.8405813 0.8897534 0.9243816
# attr(,"label")
# [1] "Correlation structure:"


#PACF for the two models
layout(matrix(1:2, ncol = 2))
pacf(resid(gam_6_ar0$lme, type = "normalized"), lag.max = 48, main = "pACF of gam n.6")
pacf(resid(gam_6_ar1$lme, type = "normalized"), lag.max = 48, main = "pACF of gam n.6 with AR(1)")


#Arima on the Residuals of the gam_6_ar0 to get the optimal p and q values. 
library(forecast)
arma_res <- auto.arima(resid(gam_6_ar0$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef
# p = 1, q = 2


#Models again with the new values of pa and q 
gam_6_arma<- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
                  data = matrix_gam,
                  family = gaussian,
                  correlation = corARMA(form = ~ 1|Weekly, p = 1, q = 2), #Values here
                  method = "REML")

anova(gam_6_ar0$lme, gam_6_ar1$lme, gam_6_arma$lme)
#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# gam_6_ar0$lme      1 10 8333.865 8378.908 -4156.933                        
# gam_6_ar1$lme      2 11 7584.062 7633.609 -3781.031 1 vs 2 751.8037  <.0001
# gam_6_arma$lme     3 13 7580.533 7639.089 -3777.267 2 vs 3   7.5282  0.0232
# gam_6_arma is better since it has the lowest AIC vaule


datas <- data.table(Fitted_values = c(gam_6_ar0$gam$fitted.values,
                                      gam_6_arma$gam$fitted.values),
                    Residuals = c(gam_6_ar0$gam$residuals,
                                  gam_6_arma$gam$residuals),
                    Model = rep(c("Gam n.6", "Gam n.6 with ARMA(1,2)"), each = nrow(data_r)))

ggplot(data = datas,
       aes(Fitted_values, Residuals)) +
  facet_grid(Model~., switch = "y") +
  geom_point(size = 1.7) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(color = "black")) +
  labs(title = "Fitted values vs Residuals of two models")


save.image("Rnd_MLR+GAM.RData")



#
#
#
# SLIDING WINDOW PREDICTIONS
#

DT_gam <- DT_agg
names(DT_gam)[names(DT_gam) == "VALUE"] = "value"

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


mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real))) # MAPE - Mean Absolute Percentage Error
}

n_weeks <- floor(length(n_date)/7) - 2

define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}

#Execute and Evaluate
# gam_pred_weeks <- predWeekGAM(DT_agg[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)])
# gam_err_mape <- mape(DT_agg[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), VALUE],
#                      gam_pred_weeks$Pred)


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

#Write file for faster execution
write_feather(DT_gam, "DT_gam")












