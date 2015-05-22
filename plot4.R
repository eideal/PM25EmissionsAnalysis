## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(plyr)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

## Merge the datasets using SCC as key
mergedData <- arrange(join(NEI, SCC), SCC)

## Only select rows that contain 'Coal' in the EI.Sector
rows <- grep('Coal', mergedData[,9])
filteredData <- mergedData[rows,]

## Summarize the data by year, summing the emissions
emi <- ddply(filteredData, .(year), summarize, totalEmissions = sum(Emissions))

## Open the png device
png(file='plot4.png')

## Plot the total coal emissions by year and fit a linear model
plot(emi$year, emi$totalEmissions, xaxt='n', xlab='Year', ylab='Total PM2.5 Emissions (tons)', main='Total PM2.5 Emissions from Coal Combustion by Year', pch=19)
model <- lm(emi$totalEmissions ~ emi$year)
abline(model, lwd=2, col='blue')

## Plot the relevant years as axis labels
axis(1, emi$year)

## Close the graphics device
dev.off()

