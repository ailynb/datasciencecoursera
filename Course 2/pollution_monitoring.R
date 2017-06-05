###########################################################################################################################
#                                        "AIR POLLUTION MONITORING DATA IN THE UNITED STATES"
###########################################################################################################################

# CLEAN ENVIRONMENT
# ----------------------------------------
#rm(list=ls())

# LIBRARY
# ----------------------------------------
#library(plyr)


# SET WORKING DIRECTORY
# ----------------------------------------
#setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 2/")


# FUNCTIONS
# ----------------------------------------
read_data <- function(directory="specdata", id=1:332){
        # 'directory': is a character vector of length 1 indicating the location of the CSV files
        # 'id': is an integer vector indicating the monitor ID numbers to be used
        
        dataFrame <- NULL
        for(file in sprintf("%03d", id))
                dataFrame <- rbind(dataFrame, read.csv(paste(directory,"/",file,".csv",sep="")))
        
        # - Add a column "complete_case'
        dataFrame$complete_case <- 0
        dataFrame[complete.cases(dataFrame),]$complete_case <- 1
        
        # Return a data frame
        return(dataFrame)
}

pollutantmean <- function(directory="specdata", polluntant, id=1:332){
        # 'directory': is a character vector of length 1 indicating the location of the CSV files
        # 'polluntant': is a character vector of length 1 indicating the name of the pollutant for which we will calcultate the mean; 
        # 'id': is an integer vector indicating the monitor ID numbers to be used
        
        # - Read data
        if(!identical(id,1:332))
                dataFrame <- read_data(directory, id)
        
        # Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
        df_aux <- dataFrame[,polluntant]
        return(mean(df_aux[!is.na(df_aux)]))
}

complete <- function(directory="specdata", id=1:332){
        # 'directory': is a character vector of length 1 indicating the location of the CSV files
        # 'id': is an integer vector indicating the monitor ID numbers to be used
        
        # - Read data
        if(!identical(id,1:332))
                dataFrame <- read_data(directory, id)
        
        # -  Number of completely observed cases in each data file. 
        number_ob_observed_cases <- count(dataFrame[dataFrame$complete_case==1,],"ID")
        names(number_ob_observed_cases) <- c("id", "nobs")
        
        # # - Orden
        # index <- with(number_ob_observed_cases, order(nobs))
        # number_ob_observed_cases <- number_ob_observed_cases[index, ]
        
        # Return a data frame
        return(number_ob_observed_cases)
}

corr <- function(directory="specdata", threshold=0){
        # 'directory': is a character vector of length 1 indicating the location of the CSV files
        # 'threshold': is a numeric vector of length 1 indicating the number
        # of completety observed observation (on all variables) requered to compute the 
        # correlation between nitrate and sulfate
        
        # - Read data
        if(!exists("dataFrame"))
                dataFrame <- read_data(directory)
        
        
        # - Monitories that have a number of observations greater than the threshold
        df_complete <- complete(directory)
        df_complete_InThreshold <- dataFrame[dataFrame$ID %in% (df_complete[df_complete$nobs > threshold,]$id) & complete.cases(dataFrame) , c("ID","sulfate", "nitrate") ]
        
        
        # If no monitors meet the threshold requirements, return a numeric
        # vector of length 0
        if (nrow(df_complete_InThreshold) == 0) {
                return(c(0))
        }
        
        # Return a numeric vector of correlations 
        correlation <-NULL 
        for(id in unique(df_complete_InThreshold$ID))
                correlation <- rbind(correlation, 
                                     cor(df_complete_InThreshold[df_complete_InThreshold$ID==id, "sulfate"],
                                         df_complete_InThreshold[df_complete_InThreshold$ID==id, "nitrate"],
                                         use="na.or.complete")
                )
        return(correlation)
        
}

# - Read data
if(!exists("dataFrame"))
        dataFrame <- read_data()

