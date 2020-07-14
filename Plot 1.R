library(tidyverse)
library(lubridate)
library(hms)

# Step 0 - Import data and make it useable; select relevant two days

# Import data
dataset <- read.table("household_power_consumption.txt", sep = ";")

# Correct columnnames and delete old first row
colnames(dataset) <- dataset[1,]
dataset <- dataset[-1,]

# Date column to date class
dataset[,1] <- as.Date(dataset[,1], format = "%d/%m/%y%y")

# Time column to time class
dataset[,2] <- as_hms(dataset[,2])

# Create a new first column, datetime
Date.Time <- as.POSIXct(paste(dataset$Date, dataset$Time), format="%Y-%m-%d %H:%M:%S")
dataset <- cbind(Date.Time, dataset)

# All other columns to numeric class
for (i in 3:9) {
  dataset[,i] <- as.numeric(dataset[,i])
}


# Select relevant two days
rel_data <- dataset%>%
  filter(Date == "2007-02-01" | Date == "2007-02-02")%>%
  select(!(Date:Time))


# Remove dataset to save space before making our plots
rm(dataset)

# Step 1 Plot 1
png("Plot 1.png")
hist(rel_data$Global_active_power, xlab = "Global Active Power", main = "Global Active Power")
dev.off()
plot.new()

