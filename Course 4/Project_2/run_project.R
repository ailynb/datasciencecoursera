# --------------------------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# --------------------------------------------------------------------------------------------------
rm(list=ls())


# --------------------------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------
library(ggplot2)


# --------------------------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 4/Project_2")
options(scipen=10000)

# --------------------------------------------------------------------------------------------------
# LOAD PM2.5 EMISSIONS DATA
# --------------------------------------------------------------------------------------------------
# - Unzip input file
file_name <- "data.zip"
if(file.exists(file_name)){
    unzip(file_name)
}
# - Load dataframes: National Emissions Inventory and Classification Source
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
NEI <- transform(NEI, year=factor(year))

# - Slightly processed data
table(NEI$year)
table(NEI_SCC$Data.Category)

unique(SCC$SCC.Level.One)
unique(SCC$SCC.Level.Two)
unique(SCC$SCC.Level.Three)
unique(SCC$SCC.Level.Four)

# --------------------------------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base
#   plotting system, make a plot showing the total PM2.5 emission from all sources for each of the 
#   years 1999, 2002, 2005, and 2008.
# --------------------------------------------------------------------------------------------------
emissions_by_year <- as.table(with(NEI, tapply(Emissions, year, sum)))

plot.new()
dev.copy(png,'plot1.png', width = 480, height = 480)
barplot(emissions_by_year, col="darkblue",
        ylab="PM2.5 Emissions", xlab="Year", main="PM2.5 Emissions From All US Sources"
) 
dev.off()    
### Conclusion: As we can see from the plot, total emissions have decreased in the US from 1999 to 2008.


# --------------------------------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from
#   1999 to 2008? Use the base plotting system to make a plot answering this question.
# --------------------------------------------------------------------------------------------------
NEI_24510 <- subset(NEI, fips=="24510")
emissions_by_year <- as.table(with(NEI_24510, tapply(Emissions, year, sum)))

plot.new()
dev.copy(png,'plot2.png', width = 480, height = 480)
barplot(emissions_by_year, col="darkblue",
        ylab="PM2.5 Emissions", xlab="Year", 
        main="PM2.5 Emissions From All Baltimore City Sources"
) 
dev.off()  
### Conclusion: Overall total emissions from PM2.5 have decreased in Baltimore City, Maryland from 
### 1999 to 2008.


# --------------------------------------------------------------------------------------------------
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#   Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
#   plot answer this question.
# --------------------------------------------------------------------------------------------------
plot.new()
dev.copy(png,'plot3.png', width = 480, height = 480)
ggplot(NEI_24510, aes(x=year,y=Emissions, fill=type))+
    geom_bar(stat="identity")+
    facet_grid(. ~ type) +
    ylab("PM2.5 Emissions") + xlab("Year") + 
    ggtitle("PM2.5 Emissions from Baltimore City by Source Type") +
    theme_bw() +
    theme(
        plot.title = element_text(size=12,lineheight=.7, face="bold", hjust = 0.5, vjust = -1.0, 
                                  color = "black"),
        legend.position="none"
    )  
dev.off()  
### Conclusion: The non-road, nonpoint, on-road source types have all seen decreased emissions 
### overall from 1999-2008 in Baltimore City.
### The point source saw a slight increase overall from 1999-2008. Also note that the point source 
### saw a significant increase until 2005 at which point it decreases again by 2008 to just above 
### the starting values.


# --------------------------------------------------------------------------------------------------
# Across the United States, how have emissions from coal combustion-related sources changed from
#   1999-2008?
# --------------------------------------------------------------------------------------------------
# Note: The sources are categorized in a few different ways from more general to more specific.
# We assume that coal combustion related SCC records are those where SCC.Level.One contains the 
# substring 'Combustion' and SCC.Level.Four contains the substring 'coal'.

scc_combustion <- grepl("Combustion", SCC$SCC.Level.One, ignore.case=TRUE)
scc_coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
scc_combustionCoal <- SCC[scc_combustion & scc_coal,]$SCC
NEI_CombustionCoal <- NEI[NEI$SCC %in% scc_combustionCoal,]

plot.new()
dev.copy(png,'plot4.png', width = 480, height = 480)
ggplot(NEI_CombustionCoal, aes(x=year,y=Emissions))+
    geom_bar(stat="identity")+
    ylab("PM2.5 Emissions") + xlab("Year") + 
    ggtitle("PM2.5 Coal Combustion Source Emission") +
    theme_bw() +
    theme(
        plot.title = element_text(size=12,lineheight=.7, face="bold", hjust = 0.5, vjust = -1.0, 
                                  color = "black"),
        legend.position="none"
    )  
dev.off()  
### Conclusion: Emissions from coal combustion related sources have decreased from 6 * 10^6 to 
### below 4 * 10^6 from 1999-2008.


# --------------------------------------------------------------------------------------------------
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? Upload a
#   PNG file containing your plot addressing this question.
# --------------------------------------------------------------------------------------------------
# Note: The sources are categorized in a few different ways from more general to more specific.
# We assume that is anything like Motor Vehicle in SCC.Level.Two.

vehicles <- grepl("Vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
scc_vehicle <- SCC[vehicles,]$SCC
NEI_MotorVehicle <- NEI[NEI$SCC %in% scc_vehicle,]

NEI_MotorVehicle_24510 <- NEI_MotorVehicle[NEI_MotorVehicle$fips=="24510",]

plot.new()
dev.copy(png,'plot5.png', width = 480, height = 480)
ggplot(NEI_MotorVehicle_24510, aes(x=year,y=Emissions))+
    geom_bar(stat="identity")+
    ylab("PM2.5 Emissions") + xlab("Year") + 
    ggtitle("PM2.5 Motor Vehicle Source Emissions in Baltimore City") +
    theme_bw() +
    theme(
        plot.title = element_text(size=12,lineheight=.7, face="bold", hjust = 0.5, vjust = -1.0, 
                                  color = "black"),
        legend.position="none"
    )  
dev.off()  
### Conclusion: Emissions from motor vehicle sources have dropped from 1999-2008 in Baltimore City!



# --------------------------------------------------------------------------------------------------
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
#   sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes
#   over time in motor vehicle emissions? Upload a PNG file containing your plot addressing this 
#   question.
# --------------------------------------------------------------------------------------------------
NEI_MotorVehicle_24510_06037 <- NEI_MotorVehicle[NEI_MotorVehicle$fips=="24510" | 
                                                     NEI_MotorVehicle$fips=="06037",]
NEI_MotorVehicle_24510_06037$city_name <-"Baltimore City"
NEI_MotorVehicle_24510_06037[NEI_MotorVehicle_24510_06037$fips=="06037",]$city_name <-"Los Angeles"


plot.new()
dev.copy(png,'plot6.png', width = 480, height = 480)
ggplot(NEI_MotorVehicle_24510_06037, aes(x=year,y=Emissions, fill=city_name))+
    geom_bar(stat="identity")+
    facet_grid(. ~ city_name) +
    ylab("PM2.5 Emissions") + xlab("Year") + 
    ggtitle("PM2.5 Motor Vehicle Source Emissions in Baltimore City & Los Angeles") +
    theme_bw() +
    theme(
        plot.title = element_text(size=12,lineheight=.7, face="bold", hjust = 0.5, vjust = -1.0, 
                                  color = "black"),
        legend.position="none"
    )  
dev.off() 
### Conclusion: Los Angeles County has seen the greatest changes over time in motor vehicle emissions.







