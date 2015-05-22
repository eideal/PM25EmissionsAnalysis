## Of the 4 types of sources indicated by the 'type' (point, nonpoint, onroad, nonroad) variable,
## which of these has seen decreases or increases in emissions from 1999-2008 for Baltimore City, Maryland 
## (fips == 24510)

library(ggplot2)
library(plyr)

## Read the RDS files
NEI <- readRDS('summarySCC_PM25.rds')

## Filter for Baltimore data
balt <- filter(NEI, fips == '24510')

## Summarize emissions by year and by type, summing the emissions
emi <- ddply(balt, .(year,type), summarize, totalEmissions = sum(Emissions))

## Order the data frame by type
emi <- emi[order(emi$type),]

## Open the png device
png(file='plot3.png')

## Make the plot with facets by type of pollution source
print(qplot(year, totalEmissions, data= emi, facets= .~ type) +
              theme(axis.text.x = element_text(angle = 90)) +
              scale_x_continuous(breaks=c(1999,2002,2005,2008)) + 
              labs(x='Year', y='Total PM2.5 Emissions (tons)', title='Total PM2.5 Emissions by Type and Year in Baltimore City') + 
              geom_point(aes(color=type)) +
              geom_smooth(method='lm', se=FALSE))

## Close the device
dev.off()
