library(lubridate)

setwd("D:/ahando2/classes/SPRING 2022/CS 424/Project/Project 1")

CTA_daily <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv",sep='\t',quote="", header = TRUE)

# change first column name from `ï..station_id` to `station_id`
colnames(CTA_daily)[1] <- "station_id"

# check if any of the data is missing
subset(CTA_daily , is.na(CTA_daily))

# format the date
CTA_daily <- CTA_daily[complete.cases(CTA_daily), ]
CTA_daily$date <- mdy(CTA_daily$date)


str(CTA_daily)
summary(CTA_daily)
dim(CTA_daily)

# station_names that will be saved
station_names <- c("UIC-Halsted","O'Hare Airport","Damen/Milwaukee")

# save data for these 3 stations in each own files
for (station_name in station_names) {
  CTA_subset <- subset(CTA_daily , stationname == station_name)
  file_name <- ifelse(station_name == "Damen/Milwaukee","Shiny/Damen_Milwaukee",paste('Shiny/', station_name, sep=""))
  write.table(CTA_subset, file=paste(file_name, ".tsv", sep=""), quote=FALSE, sep='\t')
  
}

subset(CTA_daily, stationname == "UIC-Halsted")[,"rides"]
data.frame(date=subset(CTA_daily, stationname == "UIC-Halsted")[,"date"],rides=subset(CTA_daily, stationname == "UIC-Halsted")[,"rides"])

oneYear <- subset(CTA_daily, year(date) == '2021' & stationname == "UIC-Halsted")
data <- data.frame(months=months(oneYear$date), rides=oneYear$rides)

Dates <- tapply(data$rides, data$months, FUN=sum)
as.data.frame(Dates)[1]
Rides<-as.data.frame(Dates)
data.frame(template=names(Dates),mean=Dates)
colnames(Rides)[1] <- "Rides"
month(CTA_daily$date)
