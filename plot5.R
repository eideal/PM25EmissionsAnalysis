## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City (fips == 24510)?

library(dplyr)
library(plyr)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

## Merge the datasets using SCC as key
mergedData <- arrange(join(NEI, SCC), SCC)

## Filter for Baltimore data
balt <- filter(mergedData, fips == '24510')

## Only select rows that contain 'Vehicles' in the EI.Sector
rows <- grep('Vehicles', balt[,9])
filteredData <- balt[rows,]

## Summarize the data by year, summing the emissions
emi <- ddply(filteredData, .(year), summarize, totalEmissions = sum(Emissions))

## Open the png device
png(file='plot5.png')

## Plot the total coal emissions by year
plot(emi$year, emi$totalEmissions, xaxt='n', xlab='Year', ylab='Total PM2.5 Emissions (tons)', main='Total PM2.5 Emissions from Motor Vehicles by Year\n in Baltimore City, Maryland', pch=19)
model <- lm(emi$totalEmissions ~ emi$year)
abline(model, lwd=2, col='blue')

## Plot the relevant years as axis labels
axis(1, emi$year)

## Close the graphics device
dev.off()
