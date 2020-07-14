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

# Step 2 Plot 2
png("Plot 2.png")
plot(rel_data$Global_active_power ~ rel_data$Date.Time, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)", main = "Global Active Power by time")
dev.off()
plot.new()

# Step 3 Plot 3
png("Plot 3.png")
plot(
  rel_data$Sub_metering_1 ~ rel_data$Date.Time,
  type = "l",
  xlab = "",
  ylab = "Energy sub-metering",
  main = "Energy sub-metering over time"
)
lines(
  rel_data$Sub_metering_2 ~ rel_data$Date.Time,
  col = "blue"
)
lines(
  rel_data$Sub_metering_3 ~ rel_data$Date.Time,
  col = "red"
)
legend(
  x = "topright",
  legend = c("sub_meter 1", "sub_meter 2", "sub_meter 3"),
  col = c("black", "blue", "red"),
  lty = c(1, 1, 1)
)
dev.off()
plot.new()

# Step 4 Plot 4
png("Plot 4.png")
par(mfrow = c(2,2))
plot(rel_data$Global_active_power ~ rel_data$Date.Time, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)", main = "Global Active Power by time")
plot(rel_data$Voltage ~ rel_data$Date.Time, type = "l", xlab = "", ylab = "Voltage", main = "Voltage over time")
plot(
  rel_data$Sub_metering_1 ~ rel_data$Date.Time,
  type = "l",
  xlab = "",
  ylab = "Energy sub-metering",
  main = "Energy sub-metering over time"
)
lines(
  rel_data$Sub_metering_2 ~ rel_data$Date.Time,
  col = "blue"
)
lines(
  rel_data$Sub_metering_3 ~ rel_data$Date.Time,
  col = "red"
)
legend(
  x = "topright",
  legend = c("sub_meter 1", "sub_meter 2", "sub_meter 3"),
  col = c("black", "blue", "red"),
  lty = c(1, 1, 1)
)
plot(
  rel_data$Global_reactive_power ~ rel_data$Date.Time,
  type = "l",
  xlab = "",
  ylab = "Global Reactive Power (kilowatts)",
  main = "Global reactive power over time"
)
dev.off()
