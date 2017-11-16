# --------------------------------------------------------------------------------------------------
# FIRST STEP "INSTALL"
# --------------------------------------------------------------------------------------------------
# - Install swirl
install.packages("swirl")
# - Load swirl
library(swirl)
# - Install some courses (swirl offers a variety of interactive courses) 
install_from_swirl("R Programming")
install_from_swirl("Getting and Cleaning Data")
install_from_swirl("Exploratory Data Analysis")
install_from_swirl("Statistical Inference")

# --------------------------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# --------------------------------------------------------------------------------------------------
rm(list=ls())

# --------------------------------------------------------------------------------------------------
# WORKING DIRECTORY
# --------------------------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/")

# --------------------------------------------------------------------------------------------------
# START SWIRL AND SELECT NEXT LESSON
# --------------------------------------------------------------------------------------------------
library(swirl)
swirl()
