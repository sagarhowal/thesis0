#setwd("/Users/sagarhowal/Code/Thesis/CER Electricity Revised March 2012")

library(data.table)

#Read all the Text Files
files <- list.files(pattern = "*.txt")

#Remove the OM_ID.txt file from the list
files <- files[-7]
files

data <- rbindlist(lapply(files, function(x) cbind(fread(x))))


colnames(data) <- c("METER_ID", "TIME", "VALUE")
str(data)

# This is preprocessed .csv from .xls files from Q&A forms, where is OM_ID, Type of consumer, etc.
# For analysis, I just used rezidences...
oom_id <- fread("OOM_ID.txt", header=F)
oom_id[, c("V3","V4","V5") := NULL]
colnames(oom_id) <- c("METER_ID", "Type")
table(oom_id[, Type])
rezidences <- oom_id[Type == 1, METER_ID]

# tarify <- oom_id[V1 %in% oomka, .(V3)]
# tarify <- tarify[!(V3 %in% "b"),]
# table(tarify)

DT <- data[METER_ID %in% oom_id[Type == 1, METER_ID]]

times <- data[, .N, METER_ID]
full <- times[N == 25730, .(METER_ID)]
DT <- data[METER_ID %in% full[,METER_ID],]
length(unique(DT$METER_ID))

#rez_full <- DT # run for first occasion!
rez_full <- rbindlist(list(rez_full, DT))
dim(rez_full[, .N, METER_ID])[1]

# rez_full is after reading all files, the whole data.table with all consumers with full data (no missing values)
# for clustering, or something, you need just subset data by Date, or subset of consumers...
# with data.table package it is simplest

# example ----
# subset by TIME
rez_date_sep <- copy(rez_full[TIME %in% c(24501:28448),])


#subset the DT dataframe for ID = 1951 for exploration 
DT1951 <- subset(DT, DT$METER_ID == 1951)

DT1951 <- transform(DT1951, DayEnc = substr(TIME, 1, 3), TimeEnc = substr(TIME, 4, 5))

DT1951$DayEnc <- as.numeric(DT1951$DayEnc)

DT1951$TimeEnc <- as.numeric(DT1951$TimeEnc)

summary(DT1951)

sum(ts(DT1951$TimeEnc > 48)) #4 values greater than 48. Wtf? 2 49s and 2 50s
#Correction Note: Daylight Saving Time Changes
plot(density(DT1951$DayEnc)) #Looks good. Note: Data is for more than a year.
#Lowest value for the day is 195 which means the data is from mid 2009 to around the end of 2010 (day 730). 
#googling this I found out 195th day of 14th July, 2009 Tuesday. 

#Try to fix this
#subtract 195 from the DayEnc
DT1951$DayEnc0 <- DT1951$DayEnc - 195

#[The next line]Does not work. Dates are not in the desired order. cannot paste directly. 
#DT1951 <- seq(as.POSIXct#####


DateSeq <- c(195:730)
TimeSeq <- rep(1:48)
TimeSeq <- rep(TimeSeq, length.out = length(DateSeq))
TimeSeq <- sprintf("%02d", TimeSeq)
TimeSeq <- as.numeric(TimeSeq)

Dates <- rep(0, length(DateSeq))
DateDict <- do.call(rbind.data.frame, Map('c', DateSeq, Dates))
colnames(DateDict) <- c("DateSeq", "Dates")

summary(DateDict)

DateOrig <- seq(as.Date("2009-07-14"), as.Date("2011-12-31"), by = "days")
DateOrig <- DateOrig[1:length(DateDict$DateSeq)]

#DateDict$Dates <- cbind(DateOrig[1:536])

#Noel's Code
DT1951$Date <- as.Date("2008-12-31")+as.numeric(substr(as.character(DT1951$TIME),1,3))


#Order by
DT1951New <- DT1951[order(TIME)]

#Create a dictionary for Time
TimeNum <- c(1:48)
TimeAct <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),"%H:%M", tz="GMT")
TimeAct <- TimeAct[-49]

TimeDict <- DateDict <- do.call(rbind.data.frame, Map('c', TimeNum, TimeAct))
colnames(TimeDict) <- c("TimeNum", "TimeAct")

#Join Time Dict with DT1951
join_string <- "Select a.TimeEnc, b.TimeAct from DT1951New a left join TimeDict b on a.TimeEnc = b.TimeNum"
#TimeDict a left join DT1951New b on b.TimeEnc = a.TimeNum"
library(sqldf)
DT1951New2 <- sqldf(join_string, stringsAsFactors = FALSE)

DT1951New <- cbind(DT1951New, DT1951New2$TimeAct)



#Check NAs in Dataframe
NArows <- DT1951New[rowSums(is.na(DT1951New)) > 0,]

#Trial
TimeNum2 <- c(1:50)
TimeAct2 <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),"%H:%M", tz="GMT")
TimeAct2 <- TimeAct[-49]
TimeAct2[49] <- "01:00"

DateDict2 <- do.call(rbind.data.frame, Map('c', TimeNum2, TimeAct2))
colnames(TimeDict) <- c("TimeNum", "TimeAct")




#Create Dictionary for Daylight savings time. 
DaylightDict <- data.frame("Day" = c(49,50), "Time" = c("01:00", "01:30"))


join_string2 <- "Select a.Time, b.V2 from DaylightDict a left join NArows b on a.Day = b.TimeEnc"
DaylightCorrect <- sqldf(join_string2, stringsAsFactors = FALSE)

#Add values to Vector
ts3 <- seq(as.POSIXct("2009-07-14 00:00:00", tz='Europe/Dublin'), 
           as.POSIXct('2010-12-31 23:30:00', tz='Europe/Dublin'), 
           by=1800) #1800 = 30 minutes









# # transform data
# om_mat <- rez_date_sep[, lapply(VALUE, as.vector), by = .(METER_ID)]
# om_ids <- om_mat[,1] # IDs
# # just values - time series to analyse and use TSrepr or K-means...
# om_mat_tidy <- om_mat[,-1]


#Medians and Mean Absolute  Deviation (MAD)


#Calculate for Meter_ID 1951
Med_Mad <- DT1951New[, .(Med = median(VALUE), Mad = mad(VALUE)),
                  by = (seq(nrow(DT1951New)) - 1) %% 48]
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
Med_Mad_Week <- DT1951New[, .(Med = median(VALUE), Mad = mad(VALUE)),
                       by = (seq(nrow(DT1951New)) - 1) %% (48*7)]
ggplot(Med_Mad_Week, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  #geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")


#save.image("RnD.RData")


#duplicate DT in DT_full for safety
DT_full <- DT

DT_full$DATETIME <- rep(ts)

#Daily Profile for all Consolidated 
Med_Mad_All <- DT_full[, .(Med = median(VALUE), Mad = mad(VALUE)),
                     by = (seq(nrow(DT_full)) - 1) %% 48]
library(ggplot2)
ggplot(Med_Mad_All, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad_All, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")


Med_Mad_Week_All <- DT_full[, .(Med = median(VALUE), Mad = mad(VALUE)),
                          by = (seq(nrow(DT_full)) - 1) %% (48*7)]
ggplot(Med_Mad_Week_All, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week_All, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")



#Get Day variable
# day <- c(rep("Tuesday", 48), rep("Wednesday", 48),rep("Thursday", 48), 
#          rep("Friday", 48), rep("Saturday", 48), rep("Sunday", 48), rep("Monday", 48))

DT_full[order(DT_full$METER_ID, DT_full$TIME)]

library(plyr)
arrange(DT_full,METER_ID,TIME)

DT_full$Date <- NA
DT_full$Date <- ts





