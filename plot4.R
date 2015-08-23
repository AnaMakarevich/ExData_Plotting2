#Question 4
#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Find coal combustion-related sources
# Create two huge logical matrices: first with TRUE for values that contain the word coal,
# the second one with TRUE for values that contain the word "comb"
#I noticed that it could be either Comb or Comust or Combustion, so I selected all of them
coal = as.data.frame(sapply(SCC, function (x) grepl("[^\\b*]coal ",x, ignore.case=TRUE)))
comb = as.data.frame(sapply(SCC, function (x) grepl("Comb.*",x, ignore.case=TRUE)))

# Select the first column
selvect = coal[,1]
selvect2 = comb[,1]
# Run a loop over columns to create 2 logical vectors that combine ALL the TRUE values for the two columns
for (i in 2:ncol(coal)) {
        selvect = selvect | coal[,i]
        selvect2 = selvect2 | comb[,i]
}
# Combine the two column into one - now it will hold TRUE only for the rows that contain both 'coal' and 'combustion'
selvect = selvect & selvect2

# Extract the corresponding codes
codes = SCC$SCC[selvect]

# Select only the data with these codes:
coalData = (NEI[NEI$SCC %in% codes,])

#Prepare data for plotting
summedCoalData = summarize(group_by(coalData,year), sum(Emissions))
colnames(summedCoalData) = c("Year", "Emissions")

# Create the bar plot
png("plot4.png", width = 600, height = 400)
barplot(height=summedCoalData$Emissions,
        xlab = "Year", 
        ylab = expression("Emissions from PM"[2.5]*", tons"),
        names.arg = summedCoalData$Year,
        main = expression("Total emisssions from PM"[2.5]* " from coal combustion-related sources by year"),
        col = "mistyrose")                
dev.off()