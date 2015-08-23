# Question 6
#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Create two huge logical matrices: one that has TRUE for all values that contain the word 'motor',
# the other that has TRUE for all values that contain the word starting with "veh" (for vehicle)
motor = as.data.frame(sapply(SCC, function (x) grepl("motor",x, ignore.case=TRUE)))
motor2 = as.data.frame(sapply(SCC, function (x) grepl("veh",x, ignore.case=TRUE)))

# Now combine all the columns (which are logical vectors now" using "OR":
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

#Select the observations for LA and Baltimore
LAandBaltimore = subset(NEI, fips == "24510" | fips == "06037")

#Convert fipt to factor variable with appropriate labels
LAandBaltimore$fips = as.factor(LAandBaltimore$fips)
levels(LAandBaltimore$fips) = list("Baltimore" = "24510", "Los Angeles" = "06037")

#Subset the data for motor vehicles
motorDataLAandBaltimore = (LAandBaltimore[LAandBaltimore$SCC %in% codes,])

library(dplyr)
library(ggplot2)
#Prepare the data for plotting
summedData = summarize(group_by(motorDataLAandBaltimore, year, fips), sum(Emissions))
colnames(summedData) = c("Year", "City", "Emissions")
summedData$Year = as.factor(summedData$Year)

#Create the bar blot
png("plot6.png", width = 600, height = 400)
g = ggplot(summedData, aes(x = Year, y = Emissions))
g + theme_light(base_size=12) +
        facet_wrap( ~ City) +
        geom_point(size=2, color= "black")+
        geom_smooth(size = 1, aes(group=1), stat="identity",color = "springgreen4") +
        #geom_bar(stat="identity", fill="palevioletred3") +        
        labs(title = expression("Emissions from PM"[2.5]* " from motor vehicles in LA and Baltimore City by year")) +
        labs(y = expression("Emissions from PM"[2.5]* ", tons"))        

dev.off()        