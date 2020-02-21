dat <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))


dat$Date <- as.Date(dat$Date, "%d/%m/%Y")


dat <- subset(dat,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))


dat <- dat[complete.cases(dat),]


dateTime <- paste(dat$Date, dat$Time)


dateTime <- setNames(dateTime, "DateTime")


dat <- dat[ ,!(names(dat) %in% c("Date","Time"))]


dat <- cbind(dateTime, dat)


dat$dateTime <- as.POSIXct(dateTime)

hist(dat$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

plot(dat$Global_active_power~dat$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

with(dat, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(dat, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

