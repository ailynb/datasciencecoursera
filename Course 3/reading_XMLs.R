# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())

# ------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------
library(XML)
library(plyr)

# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 3/")

# ------------------------------------------------------------------------------
# PART 1: FILE XML SIMPLE
# ------------------------------------------------------------------------------
# - READ XML
file_data <- xmlTreeParse("simple.xml", useInternalNodes = TRUE)
rootNode <- xmlRoot(file_data)
xmlName(rootNode)
names(rootNode)

# - PRINT  
rootNode[[1]]
rootNode[[1]][[1]]

# - GET THE ITEMS ON THE MENU AND PRICES  
xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)


# ------------------------------------------------------------------------------
# PART 2: FILE HTLM
# ------------------------------------------------------------------------------
# - READ FILE
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
file_data <- htmlTreeParse(fileUrl, useInternalNodes = TRUE)

# - EXTRACT CONTENT BY ATTRIBUTES
scores <- xpathSApply(file_data, "//li[@class='score']",xmlValue)
teams <- xpathSApply(file_data, "//li[@class='team-name']",xmlValue)


# ------------------------------------------------------------------------------
# PART 3: FILE XML RESTAURANTS
# ------------------------------------------------------------------------------
# - READ XML
file_data <- xmlTreeParse("restaurants.xml", useInternalNodes = TRUE)
rootNode <- xmlRoot(file_data)
xmlName(rootNode)
names(rootNode)

# - PRINT  
rootNode[[1]]
rootNode[[1]][[1]]

# - GET zipcode
zipcode_21231 <- xpathSApply(rootNode,"//zipcode",xmlValue)
count(zipcode_21231=="21231")
length(xpathApply(doc, "//zipcode[text()='21231']", xmlValue))








