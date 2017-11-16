# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())


# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 4/Clustering - Case Study 2/")


# ------------------------------------------------------------------------------
# SLIGHTLY PROCESSED DATA
# ------------------------------------------------------------------------------
# - Download and Unzip Data
unzip("pm25_data.zip")

# - Read data
data_from_1999 <- read.table("./pm25_data/RD_501_88101_1999-0.txt", comment.char="#", header=FALSE,
                             sep="|", na.strings="", stringsAsFactors = FALSE)
data_from_2012 <- read.table("./pm25_data/RD_501_88101_2012-0.txt", comment.char="#", header=FALSE,
                             sep="|", na.strings="", stringsAsFactors = FALSE)

# - Take columns names
col_names <- readLines("./pm25_data/RD_501_88101_1999-0.txt",1)
col_names <- strsplit(col_names, "|", fixed = TRUE)

# - Add columns names but removing blanks
names(data_from_1999) <- make.names(col_names[[1]])
names(data_from_2012) <- make.names(col_names[[1]])

# - Simple Value Summary
summary(data_from_1999$Sample.Value)
summary(data_from_2012$Sample.Value)

# - % of na
mean(is.na(data_from_1999$Sample.Value))
mean(is.na(data_from_2012$Sample.Value))

# - Boxplot
boxplot(data_from_1999$Sample.Value, data_from_2012$Sample.Value)
boxplot(log10(data_from_1999$Sample.Value), log10(data_from_2012$Sample.Value))

# - Negative only on 2012
negative <- (data_from_2012$Sample.Value) < 0
sum(negative, na.rm = TRUE)
mean(negative, na.rm=TRUE)

# - Dates with values negatives
dates <- as.Date( as.character(data_from_2012$Date), "%Y%m%d")
hist(dates[negative], "month")

# - Add Column "County Site"
data_from_1999$County.site <- with(data_from_1999,paste(County.Code,Site.ID,sep="."))
data_from_2012$County.site <- with(data_from_2012,paste(County.Code,Site.ID,sep="."))

# - Change format column
data_from_1999$Date <- as.Date( as.character(data_from_1999$Date), "%Y%m%d")
data_from_2012$Date <- as.Date( as.character(data_from_2012$Date), "%Y%m%d")



# ------------------------------------------------------------------------------
# FIND SOMETHING MATTER IN PM BEETWEN THIS YEARS
# ------------------------------------------------------------------------------
# - Take a single monitor location, an looking for possible changes in PM levels
site0 <- unique(subset(data_from_1999, State.Code==36, c(County.Code,Site.ID)))
site0 <- paste(site0[,1],site0[,2],sep=".")
site1 <- unique(subset(data_from_2012, State.Code==36, c(County.Code,Site.ID)))
site1 <- paste(site1[,1],site1[,2],sep=".")

# - Commun locations
both <- intersect(site0,site1)

# - Filter data
cnt0 <- subset(data_from_1999, State.Code==36 & County.site %in% both)
cnt1 <- subset(data_from_2012, State.Code==36 & County.site %in% both)

# - Total of rows by Country Site
sapply(split(cnt0, cnt0$County.site), nrow)
sapply(split(cnt1, cnt1$County.site), nrow)

# - Filter data:  We want to examine a monitor with a reasonable number of measurements so let's
#look at the monitor with ID 63.2008.
pm0sub <- subset(data_from_1999, State.Code==36 & County.Code==63 & Site.ID==2008)
pm1sub <- subset(data_from_2012, State.Code==36 & County.Code==63 & Site.ID==2008)

# - Plot 
rng <- range(pm0sub$Sample.Value,pm1sub$Sample.Value, na.rm=TRUE)
par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(pm0sub$Date, pm0sub$Sample.Value, pch=20, col="red", ylim=rng)
abline(h=median(pm0sub$Sample.Value, na.rm=TRUE),lwd=2)
plot(pm1sub$Date, pm1sub$Sample.Value, pch=20, col="blue", ylim=rng)
abline(h=median(pm1sub$Sample.Value, na.rm=TRUE))


# - Calculate the average value of PM by State
mp0 <- with(data_from_1999, tapply(Sample.Value, State.Code, mean, na.rm=TRUE))
summary(mp0)
mp1 <- with(data_from_2012, tapply(Sample.Value, State.Code, mean, na.rm=TRUE))
summary(mp1)
mrg <- merge( data.frame(state=names(mp0), mean_1999=mp0), data.frame(state=names(mp1), mean_2012=mp1),
              by = "state")
par(mfrow=c(1,1))
with(mrg, plot(rep(1999,52), mrg[,2], col=state, xlim=c(1995,2015)))
with(mrg, points(rep(2012,52), mrg[,3], col=state))
segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])
legend("bottomleft",legend=unique(mrg$state),col=unique(mrg$state),pch=1)
### Conclution: We see from the plot that the vast majority of states have indeed improved their 
###             particulate matter counts so the general trend is downward. There are a few 
###             exceptions. (The topmost point in the 1999 column is actually two points that had 
###             very close measurements.)


# which states had higher means in 2012 than in 1999. Find the rows of mrg with this particulate property.
mrg[mrg$mean.x < mrg$mean.y, ] 
### Conclution: Only 4 states had worse pollution averages, and 2 of these had means that were very 
###             close. 








# Our goal is to see if there's been a noticeable decline in this type of air pollution between these two years.

