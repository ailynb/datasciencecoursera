# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())

# ------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 3/Project/")


# ------------------------------------------------------------------------------
# GET DATA
# ------------------------------------------------------------------------------
# - Download and Unzip Data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file_name <- "UCI HAR Dataset.zip"
if(!file.exists(file_name)){
    download.file(url, file_name)
    unzip("UCI HAR Dataset.zip")
}


# ------------------------------------------------------------------------------
# READ INPUTs FILES
# ------------------------------------------------------------------------------

# - Read the list of variables of each feature vector
feature_labels <- read.csv("./UCI HAR Dataset/features.txt",sep=" ",header = FALSE, 
                           stringsAsFactors = FALSE)
setnames(feature_labels, names(feature_labels), c("feature_num", "feature_name"))
# -- For matching with all_sets' columns, add "V" to column feature_num 
feature_labels$feature_code <- paste0("V", feature_labels$feature_num)

# - Read Activity Labels 
activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt",sep=" ",header = FALSE, 
                            stringsAsFactors = FALSE, col.names = c("activity_num","activity_label"))

# -- Read Subject Files
subjects <- rbind(
    read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE,
               stringsAsFactors = FALSE, col.names = c("subject")),
    read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, 
               stringsAsFactors = FALSE, col.names = c("subject"))
)


# -- Read Activities Files
activities <- rbind(
    read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE, stringsAsFactors = FALSE, 
             col.names = c("activity_num")),
    read.csv("./UCI HAR Dataset/test/y_test.txt",header = FALSE, stringsAsFactors = FALSE, 
             col.names = c("activity_num"))
)

# -- Read Features Files
features <- rbind(
    read.table("./UCI HAR Dataset/train/x_train.txt", header = FALSE, stringsAsFactors = FALSE),
    read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, stringsAsFactors = FALSE)
)



# ------------------------------------------------------------------------------
# 1- Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
# - Ensure that the column names are unique, otherwise we'll encounter errors later when merging
colnames(features) <- make.names(feature_labels$feature_name, unique=TRUE)

# - Add the activity, subject feature data together
all_sets <- data.table(cbind(subjects, activities, features))

# -- Check the structure
dim(all_sets)

# -- Set Key
setkey(all_sets, subject, activity_num)



# ------------------------------------------------------------------------------
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------------------------
# - Create a new table containing only columns with mean and std measurements
condition_col <- colnames(all_sets)[grepl("subject|activity_num|mean|std", colnames(all_sets))]
sets_mean_std <- all_sets[, condition_col, with=FALSE]

# Check the structure
dim(sets_mean_std)



# ------------------------------------------------------------------------------
# 3- Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------
sets_mean_std <- data.table(merge(activity_labels, sets_mean_std, by="activity_num", all.y = TRUE ))

# -- Set Key
setkey(sets_mean_std, activity_num, activity_label, subject)

# -- Exclude the activity_num field
sets_mean_std <- select(sets_mean_std, -activity_num)

# Check the structure
dim(sets_mean_std)


# ------------------------------------------------------------------------------
# 4- Appropriately labels the data set with descriptive variable names 
#       - Delete problems of messy data
# ------------------------------------------------------------------------------

# Get the column names and make them unique
colnames <- make.names(colnames(sets_mean_std), unique=TRUE)

#Cleanup the variable names by replacing characters
colnamesclean<-gsub("-", " ", colnames) #Rplace - with a space
colnamesclean<-gsub("\\.", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("tBody", "Body", colnamesclean) #Remove the t
colnamesclean<-gsub("tGravity", "Gravity", colnamesclean) #Remove the t
colnamesclean<-gsub("fBody", "Body", colnamesclean) #Remove the f
colnamesclean<-gsub("BodyBody", "Body", colnamesclean) #Remove double Body
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean) #Strip leading and trailing spaces

# Recreate the column names for the dataset
colnames(sets_mean_std) <- colnamesclean

# Check the structure
str(sets_mean_std)



# ------------------------------------------------------------------------------
# 5- From the data set in step 4, creates a second, independent tidy data set with  
#       the average of each variable for each activity and each subject.
# ------------------------------------------------------------------------------
# Get the column names and make them unique
colnames(sets_mean_std) <- make.names(colnames(sets_mean_std), unique=TRUE)

set_tidy <- 
    # Create a datafram table (Dplyr) 
    tbl_df(sets_mean_std) %>% 
    # Group the data by subject and activity
    group_by(subject, activity_label) %>% 
    # Calculate the mean for all features using a Dplyr function
    summarise_each(funs(mean))


# Check the first 5 rows and 6 columns
set_tidy[1:5, 1:6]

# Create tidy dataset from step 5
write.table(set_tidy, file="set_tidy_mean.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)
