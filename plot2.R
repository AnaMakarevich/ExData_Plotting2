#Question 2

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

#Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Prepare the data for plotting
library(dplyr)
baltimoreData = subset(NEI, fips == "24510")
emissionsByYear = summarize(group_by(baltimoreData, year), sum(Emissions))
colnames(emissionsByYear) = c("year", "emissionsTotal")

# Create the barplot
png("plot2.png", width = 600, height = 400)
barplot(height=emissionsByYear$emissionsTotal,
        xlab = "Year", 
        ylab = expression("Emissions from PM"[2.5]*", tons"),
        names.arg = emissionsByYear$year, 
        main = expression("Total emisssions in Baltimore City from PM"[2.5]*" by year"),
        col = "royalblue1")                
dev.off()