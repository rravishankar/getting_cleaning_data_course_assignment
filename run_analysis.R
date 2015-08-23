#The approach we have taken is a step by step approach
#Just to understand a background first
#The measurements are in two folders
# test and train. In each of these folders
#basically all measurements are in the "X_..txt" file,
#Corresponding activities (like Walking, Laying etc.) during which these measurements 
#were taken are in the "y_..txt" file and 
#The respective subject (the id of the person on whom these measurements were taken)
#are in the "subject_..txt" file 

#First we merge these three files together in each of the folders
#by giving subject and activity proper headings so that
#headings (like V1 which R generates by default) are not mixed up/duplicated
#Then each of them contain data about the same measurements, we column 
#merge them using cbind
#Finally we row merge test and train data as required by the assignment
#To make better sense out of the data
# we label all the measurements as well in a descriptive way
# We use the measurements description for each of the 561 activities as 
# it is in the features.txt file supplied with the assignment.

#For each measurement the standard deviation and mean are already computed 
# and available as a variable (column) as decribed in features_info.txt again 
# supplied with the assignment

#Finding mean and standard deviation for each variable
#we simply use the grepl function to find the text "mean(" or "std(" in the column names
#names

#For labelling each of the acvtivity names in the table in which we already
# have the activity_id we simply perform a join with the 
# data frame corresponding to activity_labels.txt, this adds a column
# for all the activity names too


#Finally we use the aggregate function to find 
#the average of each variable for each activity and each subject
#which is the goal of this exercise !


#PLEASE NOTE WE HAVE RETAINED DATA FRAMES IN INTERMEDIATE STAGES WITH 
#DIFFERENT NAMES
# JUST FOR THE SAKE OF TESTING INDEPENDENTLY &  BETTER UNDERSTANDING
#ACTUALLY IT MAY NOT BE NEEDED - IN CASE THE DATASETS ARE LARGER
#THEN WE SHOULD (and CAN) DO AWAY  WITH ALL INTERMEDIATE DATA FRAME VARIABLES





# Test portion first

#No headers in any of the files

#Read the measurements from the X_test.txt file
X_test <- read.table("test/X_test.txt", header = FALSE)

#Read the activity labels for each of the measurements (in X_test) into the Y_test
Y_test <- read.table("test/y_test.txt", header = FALSE)
#Rename the column name to Activity_id
library(dplyr)
Y_test <- rename(Y_test, Activity_id = V1)

#Read the subject for each of the measurements (in the X_test) into subj_test
subj_test <- read.table("test/subject_test.txt", header = FALSE)
#Rename column name to Subject_id
subj_test <- rename(subj_test, Subject_id = V1)




#All three of the above are same number of rows and we are essntially 
#going to column bind them to make one data frame

#Before binding check to make sure they've the same number of rows
nrow(X_test)
nrow(Y_test)
nrow(subj_test)

#2947 for  data in the test directory

#Bind subject, activity and measurements 
test_data <- cbind(subj_test, Y_test, X_test)

# head(test_data)
# tail(test_data)

#Training data now

#Read the measurements from the X_train.txt file
X_train <- read.table("train/X_train.txt", header = FALSE)

#Read the activity labels for each of the measurements (in X_train) into the Y_train
Y_train <- read.table("train/y_train.txt", header = FALSE)
#Rename the column name to Activity_id
#library(dplyr)
Y_train <- rename(Y_train, Activity_id = V1)

#Read the subject for each of the measurements (in the X_train) into subj_train
subj_train <- read.table("train/subject_train.txt", header = FALSE)
#Rename column name to Subject_id
subj_train <- rename(subj_train, Subject_id = V1)




#All three of the above are same number of rows and we are essntially 
#going to column bind them to make one data frame

#Before binding check to make sure they've the same number of rows
nrow(X_train)
nrow(Y_train)
nrow(subj_train)


#Bind subject, activity and measurements 
train_data <- cbind(subj_train, Y_train, X_train)




#Combine both training and test data now, column names are identical

merged_data <- rbind(train_data, test_data)
#Part 1 of assignment over


#Beginning of part 2 of assignment (as also part #4 - labeling variable names)

#Name the activity labels
act_label_names <- read.table("features.txt")



act_label_names_vector <- act_label_names$V2
#COnvert to character vector
act_label_names_vector <- as.character(act_label_names_vector)

#Add the first two column names for subject and activity 
merged_data_col_names_vector <- c("Subject_id", "Activity_id", c(act_label_names_vector))


#Rename the columns, it will retain the first two column names as
#we had earlier named
colnames(merged_data) <- merged_data_col_names_vector


#Selects only the columns with standard deveition or mean - that is name ends with 'mean(' or 'std('

only_means_std <- merged_data[,grepl("(?:mean|std)\\(", colnames(merged_data))]

#Optional merged with activity and subject id to make this complete

only_means_std$Subject_id <- merged_data$Subject_id
only_means_std$Activity_id <- merged_data$Activity_id

#Completes part #2 of assignment as also part #4
# str(only_means_std)
# head(only_means_std,2)
# tail(only_means_std,2)

#Start of part #3

#Read the activity labels
activity_labels <- read.table("activity_labels.txt", header = FALSE)

#Add the corresponding columns from the activity labels to our data
merged_activity_named_data <- merge(merged_data, activity_labels, by.x = "Activity_id", by.y = "V1", all = FALSE)

#Now we have 564 columns (one more column at the end named V2 which is the activity name)
#Let's Rename the V2 column i.e. #564 to Activity_Name
colnames(merged_activity_named_data)[564] <- "Activity_Name"
#Verify it's done correctly
colnames(merged_activity_named_data)[564]


#End of part 3


#Part 4 is already done above

#Beginning of part 5

#Columns 3 to 563 is what we are interested in averaging (mean)
# And group by are Subject_id and Activity_Name
#Use the powerful aggregate function
final_data <- aggregate(merged_activity_named_data[, names(merged_activity_named_data)[3:563]], by = list(merged_activity_named_data$Subject_id, merged_activity_named_data$Activity_Name), function(x) c(mean = mean(x)))

#head(final_data)
#Group.1 is Subject
#Group.2 is Activity
#It correctly shows average of each of the variables for every activty by each of the subjects

#End of part 5
write.table(final_data, "upload_data.txt", row.name = FALSE)