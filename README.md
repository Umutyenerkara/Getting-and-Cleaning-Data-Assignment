# Coursera John Hopkins Data Science Getting and Cleaning Data Assignment

This project is meant to demonstrate the skills to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

Supplied dataset contains data collected from the accelerometers from the Samsung Galaxy S smartphone. You can download the zip file here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
Original source is here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The goal is to create R script that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Other Files

_run_analysis.R_

R script executing the steps required for this assignment. 

_codebook.MD_   

Readme file that describes data, variables and transformations done to clean the dataset.

_summary.csv_    

Independent tidy set as a CSV file describing the average of each variable for each activity and each subject.

##Important

For script to work, the locations of files in "UCI HAR Dataset" directory should be same as the original directory. 
