#Subset Dataset for a single ID
DT1951 <- subset(DT, DT$METER_ID == 1951)
DT1491 <- subset(DT, DT$METER_ID == 1491)

October25 <- subset(DT1951, DT1951$Date == "2009-10-25")

DT1951_6mon <- subset(DT1951, DT1951$DATETIME >= "2010-03-29 00:00:00" & DT1951$Date <= "2010-10-24 23:30:00" )

#Arrange
DT1951 <- DT1951[order(TIME)]


#Set up the Dataset
DT1951$DATETIME <- rep(ts)
head(DT1951)
tail(DT1951)


DT1951$Date <- as.Date("2008-12-31")+as.numeric(substr(as.character(DT1951$TIME),1,3))

library(lubridate)
DT1951[, ':='(week = weekdays(Date))]
unique(DT1951[, week])

DT_mod <- DT1951




#n_weekdays <- unique(DT1951[, week])
n_weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
n_date <- unique(DT1951_6mon[, Date])
period <- 48


#Median + Mean Absolute Deviation [Plot]
#Calculate for Meter_ID 1951
Med_Mad <- DT1951[, .(Med = median(VALUE), Mad = mad(VALUE)),
                     by = (seq(nrow(DT1951)) - 1) %% 48]
library(ggplot2)
ggplot(Med_Mad, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")


#Calculate for weekly
Med_Mad_Week <- DT1951[, .(Med = median(VALUE), Mad = mad(VALUE)),
                          by = (seq(nrow(DT1951)) - 1) %% (48*7)]
ggplot(Med_Mad_Week, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")







library(forecast)
#Define Functions for Models
#Thanks to Peter Laurinec's work on the Enernoc Dataset
# STL + ARIMA
stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  arima <- forecast(dekom, h = period, method = "arima")
  return(as.vector(arima$mean))
}
# STL + EXP
stlEXPPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN")
  return(as.vector(expo$mean))
}

#Define Evaluation Metric [MAPE]
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}


#Since Data has has dual seasonality [Daywise and Weekwise]
#Define function for seperate days and incorporate into a single one
predictWeek <- function(data, set_of_date, FUN, train_win = 6){

  for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
  for_tue <- FUN(data[(week == n_weekdays[2] & date %in% set_of_date), value])
  for_wed <- FUN(data[(week == n_weekdays[3] & date %in% set_of_date), value])
  for_thu <- FUN(data[(week == n_weekdays[4] & date %in% set_of_date), value])
  for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
  for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
  for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])

  return(c(for_mon, for_tue, for_wed, for_thu, for_fri, for_sat, for_sun))
}


#Moment of Truth
#Run Models

for_week_arima <- predictWeek(DT1951_6mon, n_date[175:202], stlARIMAPred) # forecast for one week
for_week_exp <- predictWeek(DT1951_6mon, n_date[175:202], stlEXPPred)
real_week <- DT1951_6mon[date %in% n_date[203:210], value] # real consumption
c(ARIMA = mape(real_week, for_week_arima),
  EXP = mape(real_week, for_week_exp))










