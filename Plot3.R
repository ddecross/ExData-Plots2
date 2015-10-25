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
## Question 3
## Of the four types of sources indicated by the type (point, nonpoint, onroad, 
## nonroad) variable, which of these four sources have seen decreases in emissions 
## from 1999-2008 for Baltimore City? Which have seen increases in emissions 
## from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
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
## and then only return to merged_table.melt where fips == "24510" (Maltimore City, Maryland)
merged_table.melt <- subset(melt(merged_table, id=id.vars.for.melt, measure.vars = c("Emissions")),
                            fips=="24510")
head(merged_table.melt)



## Get the sum of Emissions by year and type
tot.emissions.by.year <- dcast(merged_table.melt, year+type ~ variable, fun.aggregate = sum)
print(tot.emissions.by.year)

library(ggplot2)

png(file = "./Project Code/Exploratory Data Analysis/Project 2/plot3.png",
    width = 1600,
    height = 900)

## Set the data to be used in the plot
g <- ggplot(data=tot.emissions.by.year, aes(year, Emissions))

## set color, point size, general theme and position of the legend
g <- g + geom_point(aes(color=type, pch=type), size = 3) + theme_bw() + theme(legend.position="right")

## add smooth with linear model method as well as line type
g <- g + geom_smooth(aes(color=type), linetype = 3, method = "lm") ##, se = FALSE

## break out by type, (margins = TRUE so that we can see overall graph as well as break out by type)
g <- g + facet_grid(~type, margins=TRUE)

## add a title and x, y, and legend labels and Lable header
g <- g + labs(title="Total Emissions by Source by Year in Baltimore", x="Year", y="Total Emissions (PM25-PRI)", color="Source Type", pch="Source Type")

## fix x-axis so that it shows 1999, 2002, 2005, and 2008
g <- g + coord_cartesian(xlim=c(1998, 2009)) + 
         scale_x_continuous(breaks=tot.emissions.by.year$year) + 
         theme(axis.text.x = element_text(color="dark gray"), axis.ticks.x = element_line(color="dark gray"))

g

dev.off()

