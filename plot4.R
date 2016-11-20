plot4 <- function () {
    # Read the data into a data frame
    to_print <- sprintf("Reading data from file...")
    cat(" ", to_print, "\n")
    electric_power_consumption_data <- read.table("household_power_consumption.txt", 
                                                  sep=";",
                                                  na.strings="?",
                                                  header=TRUE)
    to_print <- sprintf("Preparing data frame...")
    cat(" ", to_print, "\n")
    # Convert the Date variable from factor to Date class
    electric_power_consumption_data$Date <- as.Date(as.character(electric_power_consumption_data$Date),
                                                    "%d/%m/%Y")
    # Create a new variable "DateTime" in the dataframe, which is a 
    # vector of string variables pasted together from the Date and Time variables
    electric_power_consumption_data$DateTime <- paste(electric_power_consumption_data$Date,
                                                      as.character(electric_power_consumption_data$Time))
    # Convert the new "DateTime" variable to POSIXlt format
    electric_power_consumption_data$DateTime <- strptime(electric_power_consumption_data$DateTime,
                                                         format="%Y-%m-%d %H:%M:%S")
    to_print <- sprintf("Subsetting data frame...")
    cat(" ", to_print, "\n")
    # Subset the dataframe to a subset of the data for only
    # February 1 and February 2 of 2007
    electric_power_consumption_data_Feb_1_and_2_2007 <- subset(electric_power_consumption_data,
                                                               (electric_power_consumption_data$DateTime$year == 106 | 
                                                                    electric_power_consumption_data$DateTime$year == 107)
                                                               & (electric_power_consumption_data$DateTime$yday >= 31 & 
                                                                      electric_power_consumption_data$DateTime$yday <= 32))
    to_print <- sprintf("Creating plot file...")
    cat(" ", to_print, "\n")
    png(file="plot4.png",width=480,height=480)
    
    par(mfrow=c(2,2))
    xrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$DateTime)
    yrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$Global_active_power)
    # 1st plot
    plot(xrange,yrange,type="n",xlab="",ylab="Global Active Power")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Global_active_power)
    # 2nd plot
    yrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$Voltage)
    plot(xrange,yrange,type="n",xlab="datetime",ylab="Voltage")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Voltage)
    # 3rd plot
    yrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$Sub_metering_1)
    plot(xrange,yrange,type="n",xlab="",ylab="Energy sub metering")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Sub_metering_1)
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Sub_metering_2,col="red")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Sub_metering_3,col="blue")
    legend("topright", lty=1, col=c("black", "red","blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    # 4th plot
    yrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$Global_reactive_power)
    plot(xrange,yrange,type="n",xlab="datetime",ylab="Global_reactive_power")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Global_reactive_power)
    dev.off()
}