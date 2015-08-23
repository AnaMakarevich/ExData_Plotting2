#Question 3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999-2008 for 
# Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

#Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Prepare the data for plotting
library(dplyr)
library(ggplot2)
# Select observations for Baltimore
baltimoreData = subset(NEI, fips == "24510")
summedData = summarize(group_by(baltimoreData,year, type), sum(Emissions))
colnames(summedData) = c("Year", "Type", "Emissions")
summedData$Year = as.factor(summedData$Year)

# Create the bar plot
png("plot3.png", width = 800, height = 600)
g = ggplot(summedData, aes(x = Year, y = Emissions))
g + theme_light(base_size=12) +
        facet_wrap( ~ Type, nrow = 2, ncol=2) +
        geom_bar(stat="identity", fill="thistle3") +        
        labs(title = expression("Total emissions from PM"[2.5]* " in Baltimore City by type, by year")) +
        labs(y = expression("Emissions from PM"[2.5]* ", tons"))+
        geom_smooth(method="lm", aes(group=1), se = FALSE, size= 1, linetype=3, color = "violetred4")
dev.off()