#Set the working directory:
setwd("/Users/Kianamon/R")
rm(list=ls())
#####################################################################################
#libraries in use:
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(vistime)
#####################################################################################
#data:
Time1 <- c("2016-05-25 17:45:00", "2016-05-26 08:50:00", "2016-05-26 13:45:00",
          "2016-05-26 16:45:00", "2016-05-27 09:20:00", "2016-05-27 15:50:00",
          "2016-05-27 17:10:00", "2016-05-27 22:30:00", "2016-05-28 09:10:00",
          "2016-05-28 13:25:00", "2016-05-28 19:10:00", "2016-05-29 08:35:00",
          "2016-05-29 17:55:00", "2016-05-29 20:35:00", "2016-05-30 17:40:00",
          "2016-05-31 08:40:00", "2016-05-31 18:10:00", "2016-05-31 18:50:00",
          "2016-05-31 20:45:00"
          )
Time <- ymd_hms(Time1)
BloodPressureSystolic <- c(123, 131, 129, 129, 113, 131, 128, 121, 
                      127, 131, 121, 126, 131, 125, 124, 
                      117, 147, 115, 121)
BloodPressureDiastolic <- c(83, 74, 82, 79, 73, 90, 81, 86, 87, 
                      86, 74, 88, 88, 79, 86, 75, 95, 75, 80)
labels <- c("", "Before Breakfast", "", "", "Before Breakfast", "IceCream",
            "Lunch", "", "Before Breakfast", "Lunch", "", "", 
            "Got Home", "", "", "", "Alcohol", "", "")
#####################################################################################
my.data <- data.frame(BloodPressureSystolic, BloodPressureDiastolic, Time, labels)
my.data %>%
  gather(key,Pressure, BloodPressureSystolic, BloodPressureDiastolic) %>%
  ggplot(aes(x=Time, y=Pressure, colour=key)) +
  geom_line() +
  geom_text(aes(label=labels), cex = 3)

