## Required files and their short explanations 

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'test/subject_text.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

## Script applies the following transformations to data and files. 

1. Reads the required files with read.table function.

2. Merges the training and test activity data in one data frame. Renames the variable as "activity". Changes the variable to factor and renames the levels using fct_recode (optional). Recoding is done in accordance with the activity.labels.txt: 
walking = "1", walking_upstairs = "2", walking_downstairs = "3", sitting = "4", standing = "5", laying = "6" 

3. Merges the training and test subject data in one data frame. Renames the variable as "id" then turns it into a factor. 

4. Merges the training and test data as merged_data. 

5. Finds the index number of columns in features set(features.txt) containing the strings "mean()" and "std()". Then discards the other columns from the merged data set using select function. 

6. Finds the relevant column names from features data set using grep function. Then passes them to merged dataset as column names using transpose function.

7. Removes parantheses and dashes from column names for better readability.

8. Merges subject_id and activity_names data together with merged data set in a new data framed named tidy_data. Then transforms it to tibble for better formatting (optional).

9. Creates a new data frame from the averages of all variables using Dplyr functions group_by and summarize_at. Adds string "mean" at the end of original variables.

10. #Writes the resulting independent data frame to a new csv file named "summary.csv" in your working directory. 
