###################################################################################################
#          "HOSPITAL COMPARE WEB SITE - U.S. Department of Health and Human Services."
#
# FILES:
# "outcome-of-care-measures.csv" : This table provides each hospital's risk-adjusted 30-Day Death 
#                                   (mortality) and 30-Day Readmission category and rate.
#
###################################################################################################

# CLEAN ENVIRONMENT
# ----------------------------------------
rm(list=ls())

# LIBRARY
# ----------------------------------------
library(plyr)
library(stringr)

# SET WORKING DIRECTORY
# ----------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 2/")


# READ FILE "outcome-of-care-measures.csv"
# ----------------------------------------
read_outcome_data <- function(){
    read.csv("hospital_compare_data/outcome-of-care-measures.csv", colClasses = "character")
    
}
outcome_data <- read_outcome_data()


# CHANGE BY VALID OUTCOME NAME
# ----------------------------------------
replace_valid_outcome <- function(outcome) {
    return (gsub("heart attack","Heart.Attack", 
                 gsub("heart failure","Heart.Failure", 
                      gsub("pneumonia","Pneumonia", outcome))))
    
}




# PART 1: Plot the 30-day mortality rates for heart attack
## Tasas de mortalidad de hospitalización de 30 días a partir de un  
## ataque cardíaco: Enumera la tasa ajustada por riesgo (porcentaje)
## para cada hospital.
# ----------------------------------------
create_histogram <- function(){
    # - To make a simple histogram of the 30-day death rates from heart attack
    if(!exists("outcome_data"))
        outcome_data <- read_outcome_data()
    
    outcome_data[, 11] <- as.numeric(outcome_data[, 11])
    hist(outcome_data[, 11],
         main="30-day death rates from heart attack",
         xlab="",
         labels=TRUE,
         col = "lightblue"
    )
}
## TEST
create_histogram()




# PART 2: Finding the best hospital in a state
# ----------------------------------------
best <- function(state, outcome) {
    ## Read outcome data
    if(!exists("outcome_data"))
        outcome_data <- read_outcome_data()
    
    outcome <- replace_valid_outcome(outcome)
    
    ## Select Columns "Hospital.Name", "State" and Outcome Columns
    hospitals <- outcome_data[,c(2,7,grep(outcome, names(outcome_data)))]
    
    ## Check that outcome is valid
    if(ncol(hospitals)==2)
        return("Invalid outcome")
    
    ## Check that state is valid
    if(nrow(hospitals[hospitals$State==state,])>0){
        
        # -  Subselect Columns "Hospital.Name", "State" and 
        #"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from."
        col_name <- "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from."
        hospitals <- hospitals[hospitals$State==state, c(1,2,grep(col_name, names(hospitals)))]
        names(hospitals) <- c("Hospital.Name","State","Lower.Rates")
        
        # - Change "Lower.Rates" Class
        hospitals$Lower.Rates <- as.numeric(hospitals$Lower.Rates)
        
        ## Return hospital name in that state with lowest 30-day death rate
        return(hospitals[!is.na(hospitals$Lower.Rates) & hospitals$Lower.Rates==
                             min(hospitals$Lower.Rates,na.rm=TRUE)
                         ,1])
        
    }else
        return("Invalid state")
    
    
}

## TEST
best("TX", "heart attack")=="CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")=="FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")=="JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")=="GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")=="Invalid state"
best("NY", "hert attack")=="Invalid outcome"




# PART 3: Ranking hospitals by outcome in a state
# ----------------------------------------
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    if(!exists("outcome_data"))
        outcome_data <- read_outcome_data()
    
    outcome <- replace_valid_outcome(outcome)
    
    ## Select Columns "Hospital.Name", "State" and Outcome Columns
    hospitals <- outcome_data[,c(2,7,grep(outcome, names(outcome_data)))]
    
    
    ## Check that outcome is valid
    if(ncol(hospitals)==2)
        return("Invalid outcome")
    
    ## Check that state is valid
    if(nrow(hospitals[hospitals$State==state,])>0){
        
        # -  Subselect Columns 
        col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome,sep="")
        hospitals <- hospitals[hospitals$State==state, c("Hospital.Name","State",col_name)]
        names(hospitals) <- c("Hospital.Name","State","Rates")
        
        # - Change "Rates" Class
        hospitals$Rates <- as.numeric(hospitals$Rates)
        
        ## Order
        index <- with(hospitals, order(Rates,Hospital.Name))
        hospitals <- hospitals[index, ]
        
        ## Remove hospitals that do not have data 
        hospitals <- hospitals[!is.na(hospitals$Rates),]
        
        ## Add Rank
        hospitals$Rank <- seq(1:nrow(hospitals))
        
        ## Change "num"
        if(num=="best"){
            num <- 1
        }else {
            if(num=="worst"){
                num <- nrow(hospitals)
            }else {
                if(num > nrow(hospitals)){
                    return(NA)
                }else 
                    num <- as.integer(num)
            }
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        return(hospitals[hospitals$Rank==num,1])
        
    }else
        return("Invalid state")
}

### TEST
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)





# PART 4: Ranking hospitals in all states
# ----------------------------------------
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    if(!exists("outcome_data"))
        outcome_data <- read_outcome_data()
    
    ## Order
    index <- with(outcome_data, order(State,Hospital.Name))
    outcome_data <- outcome_data[index, ]
    
    
    outcome <- replace_valid_outcome(outcome)
    
    ## For each state, find the hospital of the given rank
    rank_states <- data.frame("hospital"="","state"=unique(outcome_data$State),stringsAsFactors = FALSE)
    for(state in rank_states$state){
        rank_states[rank_states$state==state,]$hospital <- rankhospital(state,outcome,num)
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    return(rank_states)
}


### TEST
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)



################### QUIZ ##################
# > best("SC", "heart attack")
# [1] "MUSC MEDICAL CENTER"
# 
# > best("NY", "pneumonia")
# [1] "MAIMONIDES MEDICAL CENTER"       "NYU HOSPITALS CENTER"            "MARGARETVILLE MEMORIAL HOSPITAL"
# 
# > best("AK", "pneumonia")
# [1] "YUKON KUSKOKWIM DELTA REG HOSPITAL"
# 
# > rankhospital("NC", "heart attack", "worst")
# [1] "WAYNE MEMORIAL HOSPITAL"
# 
# > rankhospital("WA", "heart attack", 7)
# [1] "YAKIMA VALLEY MEMORIAL HOSPITAL"
# 
# > rankhospital("TX", "pneumonia", 10)
# [1] "SETON SMITHVILLE REGIONAL HOSPITAL"
# 
# > rankhospital("NY", "heart attack", 7)
# [1] "BELLEVUE HOSPITAL CENTER"
# 
# > r <- rankall("heart attack", 4)
# > as.character(subset(r, state == "HI")$hospital)
# [1] "CASTLE MEDICAL CENTER"
# 
# > r <- rankall("pneumonia", "worst")
# > as.character(subset(r, state == "NJ")$hospital)
# [1] "BERGEN REGIONAL MEDICAL CENTER"
# 
# > r <- rankall("heart failure", 10)
# > as.character(subset(r, state == "NV")$hospital)
# [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"













