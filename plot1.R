#Question 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

#Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Prepare data fro plotting:
library(dplyr)
emissionsByYear = summarize(group_by(NEI, year), sum(Emissions))
colnames(emissionsByYear) = c("year", "Emissions")

#Create the barplot
png("plot1.png", width = 600, height = 400)
barplot(height=emissionsByYear$Emissions, width = 20, 
        xlab = "Year", ylab = expression("Emissions from PM"[2.5]*" ,tons"),
        names.arg = emissionsByYear$year, 
        main = expression("Total emisssions from PM"[2.5]*" by year"),
        col = "thistle3")                
dev.off()