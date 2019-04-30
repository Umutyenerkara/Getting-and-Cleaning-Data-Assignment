library(tidyverse)

#Read required files

x_train <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/train/y_train.txt")
x_test <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/train/subject_train.txt")
featureNames <- read.table("C:/Users/Umut Yener Kara/Desktop/Coursera/Course 3/Assignment/UCI HAR Dataset/features.txt")

#Merge the training and test activity data in a data frame. Rename the variable. Change it to factor and rename the levels using fct_recode (optional).

activity_data<- rbind(y_train, y_test) 
activity_data <- rename(activity_data, activity = V1)
activity_data$activity <- as_factor(activity_data$activity)
activity_data$activity <- fct_recode(activity_data$activity, walking = "1", walking_upstairs = "2", walking_downstairs = "3", sitting = "4", standing = "5",
                                                                                    laying = "6")

#Merge the training and test subject data in a data frame. Rename it then turn it to a factor. 

subject_ids <- rbind (subject_train, subject_test)
subject_ids <- rename(subject_ids, id = V1)
subject_ids <- subject_ids %>% mutate(id = as_factor(id))

#Merge the training and test data. 

merged_data <- rbind(x_train, x_test)

#Find the index number of columns including "mean()" and "std()". Then select these columns within the merged data set. 

columns <- grep("mean()\\(\\)|std()\\(\\)",featureNames$V2, ignore.case = T)
merged_data <- merged_data %>% select(seq.int(columns))

#Find the relevant column names from featuresNames data set. Then pass them to merged dataset using transpose function.


columnsmeanstd <- filter(featureNames, grepl("mean()\\(\\)|std()\\(\\)",featureNames$V2, ignore.case = T))
columnsmeanstd <- columnsmeanstd %>% select(V2)
colnames(merged_data)[1:66] <- t(columnsmeanstd)

#Remove parantheses and dashes from column names for better readability.

colnames(merged_data) <- gsub("\\()", "", names(merged_data))
colnames(merged_data) <- gsub("-", "", names(merged_data))

#Merges subject id data and activity data together with merged data set. Then transform it to tibble (optional).

tidy_data <- cbind(subject_ids, activity_data, merged_data)
tidy_data <- as_tibble(tidy_data)

#Creates a new data frame from averages of variables using Dplyr functions named group_by and summarize_at. 

tidy_data_averages <- tidy_data %>% group_by(id, activity) %>% summarise_at(.vars = names(.)[3:68],
                                                                           .funs = c(mean="mean"))
#Writes the resulting data frame as a new csv file named "summary.csv"

write.csv(tidy_data_averages, file = "summary.csv", row.names = F )


