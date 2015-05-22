## Have total emissions from PM2.5 decreased in the U.S. from 1999 to 2008? 
## Total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 2008?

library(plyr)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')

## Summarize the data by year, summing the emissions
emi <- ddply(NEI, .(year), summarize, totalEmissions = sum(Emissions))

## Remove scientific notation when plotting
options(scipen=10)

## Open the png device
png(file='plot1.png')

## Plot the total emissions by year and fit a linear model
plot(emi$year, emi$totalEmissions, xaxt='n', xlab='Year', ylab='Total PM2.5 Emissions (tons)', main='Total PM2.5 Emissions by Year', pch=19)
model <- lm(emi$totalEmissions ~ emi$year)
abline(model, lwd=2, col='blue')

## Plot the relevant years as axis labels
axis(1, emi$year)

## Close the device
dev.off()



#Slower way to do it:
#nei_by_year <- vector("numeric")
#
#for (i in c(1999,2002,2005,2008)){
#        print(i)
#        nei_year <- NEI[NEI[,6]== i,]
#        sum <- sum(nei_year[,4])
#       nei_by_year <- c(nei_by_year, sum)
#}
#print(nei_by_year)