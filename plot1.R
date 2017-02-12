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
}

# Loading Data
project_data <- getting_data()
# Parse Date Data and Filter
project_data <- tidy_data(project_data)
# Remove private functions
rm(list = c("getting_data", "tidy_data"))

#Making Plot
png("plot1.png", height = 480, width = 480, units="px")
with(project_data, hist(Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power"))
dev.off()
print(paste(paste0(getwd(), "/plot1.png"), "was created..."))
rm(project_data)