library(install.load)
install_load("lubridate", "dplyr", "readr")

getting_data <- function(){
        #load dependencies
        file_memory_size <- file.size("household_power_consumption.txt") / 1024 / 1024
        systems_memory_limit <- memory.size(NA)
        if(systems_memory_limit < file_memory_size){
                stop("The file to be read requires more memory than the available in this CPU")
        }
        project_data <- read_delim("household_power_consumption.txt", delim = ";", col_names = T, na = "?")
        return(project_data)
}

tidy_data <- function(df){
        #transform Date and Time to proper formats
        project_data$Date <- dmy(project_data$Date)
        
        #filter data to projects requirements
        project_data <- filter(project_data, Date >= ymd(20070201), Date <= ymd(20070202))
        
        #join cloumn date with time for plot
        project_data$DateTime  <- project_data$Date + seconds_to_period(seconds(project_data$Time))
        project_data <- select(project_data, DateTime, Date:Sub_metering_3)
}

# Loading Data
project_data <- getting_data()
# Parse Date Data and Filter
project_data <- tidy_data(project_data)
# Remove private functions
rm(list = c("getting_data", "tidy_data"))

#Making Plot
png("plot3.png", height = 480, width = 480, units="px")
with(project_data, plot(DateTime, Sub_metering_1, type = "n", xlab = "", ylab = "Energy Submetering"))
with(project_data, lines(DateTime, Sub_metering_1, type = "l"))
with(project_data, lines(DateTime, Sub_metering_2, type = "l", col = "red"))
with(project_data, lines(DateTime, Sub_metering_3, type = "l", col = "blue"))
legend("topright", col = c("black", "red", "blue"), pch = rep("",3), lty=1, lwd=1, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()
print(paste(paste0(getwd(), "/plot3.png"), "was created..."))
rm(project_data)