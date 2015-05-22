## Have total emissions from PM2.5 decreased in Baltimore City, Maryland (fips == "24510") from 1999 to 2008?

library(plyr)
library(dplyr)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')

## Filter for Baltimore data
balt <- filter(NEI, fips == '24510')

## Summarize the data by year, summing the emissions
emi <- ddply(balt, .(year), summarize, totalEmissions = sum(Emissions))

## Remove scientific notation
options(scipen=10)

## Open the png device
png(file='plot2.png')

## Plot the total emissions by year and fit a linear model
plot(emi$year, emi$totalEmissions, xaxt='n', xlab='Year', ylab='Total PM2.5 Emissions (tons)', main='Total PM2.5 Emissions by Year in Baltimore City, Maryland', pch=19)
model <- lm(emi$totalEmissions ~ emi$year)
abline(model, lwd=2, col='blue')

## Plot the relevant years as axis labels
axis(1, emi$year)

## Close the graphics device
dev.off()