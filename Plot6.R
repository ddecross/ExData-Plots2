##***********************************************************************
## Exploratory Data Analysis
## Project 2
##
## Douglas DeCross
## 20151019
##
## Content of summaryScc_PM25.rds
## fips: A five-digit number (represented as a string) indicating the U.S. county
## SCC: The name of the source as indicated by a digit string (see source code classification table)
## Pollutant: A string indicating the pollutant
## Emissions: Amount of PM2.5 emitted, in tons
## type: The type of source (point, non-point, on-road, or non-road)
## year: The year of emissions recorded
##
## Question 6
## Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?
##
##**********************************************************************

## set my Working Directory
setwd("C:/Users/Doug/Coursera R WD")

## validate that the 2 needed files exist
"summarySCC_PM25.rds" %in% dir("./data/exdata-data-NEI_data") 
"Source_Classification_Code.rds" %in% dir("./data/exdata-data-NEI_data") 

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./data/exdata-data-NEI_data/Source_Classification_Code.rds")

## Convert to tables
library(data.table)
NEI <- data.table(NEI)
SCC <- data.table(SCC)

## Set up keys on tables
setkeyv(NEI, c("fips","SCC","Pollutant","Emissions","type","year"))
setkey(SCC, SCC)

tables()

## Create one table that has the 2 datasets mearged
merged_table <- merge(NEI, SCC, by=c("SCC"), all.x=TRUE)  ## 6,497,651 obs of 20 variables

## retrieve the names of the category variables (note, they are all of the variables except for "Emissions")
id.vars.for.melt <- names(merged_table[,!colnames(merged_table) %in% c("Emissions"), with=FALSE])

## melt the data.table, set "Emissions" as the measure.vars.
## and then only return to merged_table.melt where fips == "24510" (Baltimore City, Maryland)
merged_table.melt <-
    subset(melt(
        merged_table, id = id.vars.for.melt, measure.vars = c("Emissions")),
    fips %in% c("24510", "06037"))

head(merged_table.melt)
names(merged_table.melt)

## pull only records where SCC.Level.One equals 'MOBILE SOURCES' and
## SCC.Level.Two contain VEHICLE.  Use the upper function to pull all
## occurances regardless of case.  This should pull all recoreds that 
## are reffering to Vehicle as the source of the emmissions
OnlyVeh <- merged_table.melt[toupper(SCC.Level.One) == 'MOBILE SOURCES'
                      & grepl("VEHICLE",toupper(merged_table.melt$SCC.Level.Two)),]

head(OnlyVeh)
names(OnlyVeh)

## Get the sum of Emissions by year
Tot.emissions.by.year <- dcast(OnlyVeh, year+SCC.Level.Two+city ~ variable, fun.aggregate = sum)
print(Tot.emissions.by.year)

library(ggplot2)

png(file = "./Project Code/Exploratory Data Analysis/Project 2/plot6.png",
    width = 1600,
    height = 900)

## Change the legend from displaying the fips to displaying the city name
library(plyr)
Tot.emissions.by.year <- mutate(Tot.emissions.by.year, city = ifelse(Tot.emissions.by.year$fips == "24510", "Baltimore City", "Los Angeles County"))

## Set the data to be used in the plot
g <- ggplot(data=Tot.emissions.by.year, aes(year, Emissions))

## set color, point size, general theme and position of the legend
g <- g + geom_point(aes(color=city, pch=city), size = 3) + theme_bw() + theme(legend.position="right")

## add smooth with linear model method as well as line type
g <- g + geom_smooth(aes(color=city), linetype = 3, method = "lm") ##, se = FALSE

## break out by type, (margins = TRUE so that we can see overall graph as well as break out by type)
g <- g + facet_grid(. ~SCC.Level.Two, margins=FALSE)

## add a title and x, y, and legend labels and Lable header
g <- g + labs(title="Total Emissions for Motor Vehicles \nBaltimore City Compared to Los Angeles County \nby Year", x="Year", y="Total Emissions (PM25-PRI)")



## fix x-axis so that it shows 1999, 2002, 2005, and 2008
g <- g + coord_cartesian(xlim=c(1998, 2009)) + 
         scale_x_continuous(breaks=Tot.emissions.by.year$year) + 
         theme(axis.text.x = element_text(color="dark gray"), axis.ticks.x = element_line(color="dark gray"))

g

dev.off()

