## Compare emissions from motor vehicle sources in Baltimore City 
## with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

library(dplyr)
library(plyr)
library(lattice)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

## Merge the datasets using SCC as key
mergedData <- arrange(join(NEI, SCC), SCC)

## Filter for Baltimore and LA data
filteredData <- filter(mergedData, fips == '24510' | fips == '06037')

## Only select rows that contain 'Vehicles' in the EI.Sector
rows <- grep('Vehicles', filteredData[,9])
vehicleData <- filteredData[rows,]

## Summarize the data by year and by fips, summing the emissions
emi <- ddply(vehicleData, .(year,fips), summarize, totalEmissions = sum(Emissions))

## Open the png device
png(file='plot6.png')

## Make the panel lattice plot
fips_type <- factor(emi$fips, levels=unique(emi$fips), labels=c("Los Angeles", "Baltimore City"))
print(xyplot(emi$totalEmissions ~ emi$year | fips_type, layout = c(2,1), xlab='Year', 
             ylab='Total PM2.5 Emissions (tons)', 
             main='Total PM2.5 Emissions from Motor Vehicles by Year\n in Los Angeles and Baltimore City',
             panel=function(x,y,...){
                     panel.xyplot(x, y,...)
                     panel.lmline(x,y, col=2)
             }))

## Close the device
dev.off()

