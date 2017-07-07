# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())

# ------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------
library(sqldf)

# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 3/")


# ------------------------------------------------------------------------------
# READING SQLDF LIBRARY
#    The sqldf package allows for execution of SQL commands on R data frames. 
#    We will use the sqldf package to practice the queries we might send with the dbSendQuery 
#    command in RMySQL.
# ------------------------------------------------------------------------------


# 1. Download the American Community Survey data and load it into an R object called acs:
file_data <- "survey.csv"
if(!exists("survey_data"))
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",file_data)
acs <- read.csv(file_data, header = TRUE, sep = ",")


# 2. Select only the data for the probability weights pwgtp1 with ages less than 50
pwgtp1_50 <- acs[acs$AGEP<50,"pwgtp1"]
pwgtp1_50 <- sqldf("select pwgtp1 from acs where AGEP < 50")



# 3. What is the equivalent function to unique(acs$AGEP)
distinct_ages <- unique(acs$AGEP)
distinct_ages <- sqldf("select distinct AGEP from acs")



