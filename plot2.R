plot2 <- function () {
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
    png(file="plot2.png",width=480,height=480)
    
    xrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$DateTime)
    yrange <- range(electric_power_consumption_data_Feb_1_and_2_2007$Global_active_power)
    plot(xrange,yrange,type="n",xlab="",ylab="Global Active Power (kilowatts)")
    lines(electric_power_consumption_data_Feb_1_and_2_2007$DateTime,electric_power_consumption_data_Feb_1_and_2_2007$Global_active_power)
    dev.off()
    to_print <- sprintf("Done!")
    cat(" ", to_print, "\n")
    
}