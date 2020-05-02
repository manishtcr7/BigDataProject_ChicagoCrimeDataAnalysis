# Importing required libraries.
library(lubridate)
library(stringr)
# install.packages("dplyr")
library(dplyr)
library(data.table)
library(ggplot2)

#Read cleaned data
Dst <- read.csv("clean_data.csv", header = T, stringsAsFactors = F)

# Splitting Date and Time in two different Columns.
m<-str_split_fixed(Dst$Date, " ", 3)
Dst$Date <-m[,1]
Dst$Time <-m[,2]
Dst$AMPM <-m[,3]


#Splits the time as HH MM 
# split <- strsplit(Dst$Time, ":")

t<-str_split_fixed(Dst$Time, ":", 3)
Dst$Hour <-t[,1]
Dst$Minute <-t[,2]



Dst$Hour <- as.numeric(Dst$Hour)
#Converts time in 24 hour format
condition <- Dst$AMPM == "PM"
Dst$Hour[condition] <- Dst$Hour[condition] + 12

v<-str_split_fixed(Dst$Date, "/", 3)
Dst$Month <-v[,1]

# season <- c(rep("Winter", nrow(Dst)))
Dst$Season <- "Winter"

#Assigns the correct season according to the month
# for(n in 1:nrow(Dst)){
#   if(Dst$Month >= 3 & Dst$Month < 6)
#   {
#     Dst$season = "Spring"
#   }
#   else if (Dst$Month >= 6 & Dst$Month < 9)
#   {
#     Dst$season = "Summer"
#   }
#   else if (Dst$Month >=9 & Dst$Month < 12)
#   {
#     Dst$season = "Fall"
#   }
# }

Dst$Month <- as.numeric(Dst$Month)

Dst[Dst$Month >= 3 & Dst$Month < 6,]$Season <- 'Spring'
Dst[Dst$Month >= 6 & Dst$Month < 9,]$Season <- 'Summer'
Dst[Dst$Month >= 9 & Dst$Month < 12,]$Season <- 'Fall'


drops <- c("AMPM","Minute","X")
Dst <- Dst[ , !(names(Dst) %in% drops)]

write.csv(Dst,"C:/Users/manis/Desktop/SEM2/BDT/Project/CrimeDataAnalysis-master/CrimeDataAnalysis-master/new_clean_data_final.csv", row.names = FALSE)
