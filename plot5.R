#Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Create two huge logical matrices: one that has TRUE for all values that contain the word 'motor',
# the other that has TRUE for all values that contain the word starting with "veh" (for vehicle)
motor = as.data.frame(sapply(SCC, function (x) grepl("motor",x, ignore.case=TRUE)))
motor2 = as.data.frame(sapply(SCC, function (x) grepl("veh",x, ignore.case=TRUE)))

# Select the first column of each matrix
selvect = motor[,1]
selvect2 = motor2[,1]

# Run a loop over all column in the matrices and convert them into 2 vectors
# The vector has TRUE value if at least one of the values in the row is true
for (i in 2:ncol(motor)) {
        selvect = selvect | motor[,i]
        selvect2 = selvect2 | motor2[,i]
}

# Combine the two column into one - now it will hold TRUE only for the rows that contain both 'motor' and 'vehicle'
selvect = selvect & selvect2

# Extract the corresponding codes
codes = SCC$SCC[selvect]

# Subset the data for Baltimore City
baltimoreData = subset(NEI, fips == "24510")
#Subset the data with the codes for motor vehicles
motorDataBaltimore = (baltimoreData[baltimoreData$SCC %in% codes,])

# Prepare the data for plotting
summedMotorData = summarize(group_by(motorDataBaltimore,year), sum(Emissions))
colnames(summedMotorData) = c("Year", "Emissions")

png("plot5.png", width = 600, height = 400)
barplot(height=summedMotorData$Emissions,
        xlab = "Year", 
        ylab = expression("Emissions from PM"[2.5]*", tons"),
        names.arg = summedMotorData$Year,
        main = expression("Total emisssions from PM"[2.5]* " from motor vehicles in Baltimore City by year"),
        col = "olivedrab3")    
dev.off()