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
## Question 2
## Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
## system to make a plot answering this question.
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



## Get the sum of Emissions by year
tot.emissions.by.year <- dcast(merged_table.melt, year ~ variable, fun.aggregate = sum)
print(tot.emissions.by.year)

png(file = "./Project Code/Exploratory Data Analysis/Project 2/plot2.png",
    width = 480,
    height = 480)
par(mfcol = c(1,1)) ## plot 1x1 on one page

with(tot.emissions.by.year, 
     plot(year, Emissions
          ,xlab = "Year"
          ,ylab = "Total Emissions (PM25-PRI)"
          ,pch = 16    ## Set graph object to use (blue dimond)
          ,ylim = c(0,4000)
          ,xlim = c(1999,2008)
          ,xaxt = "n"))
## Set Labels
axis(side = 1, at=tot.emissions.by.year$year, labels = tot.emissions.by.year$year)
## Add the regression line
with(tot.emissions.by.year, abline(lm(Emissions ~ year), lty=1, col="blue"))

## Set legend

legend("topright"
       ,col = c("blue")
       ,lty = 1
       ,legend = c("Emissions Trend Line"))
title(main="Total Emissions For Baltimore City by Year", sub="Emissions from All Sources where FIPS = '24510'")
dev.off()

