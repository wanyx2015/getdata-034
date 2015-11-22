library(dplyr)
library(tidyr)

##
## STEP ONE: Merges the training and the test sets to create one data set.
##


## Read test data set and bind the activity and subject id
x_test <- read.table(file = "./test/X_test.txt")
y_test <- read.table(file = "./test/y_test.txt")
subject_test <- read.table(file = "./test/subject_test.txt")
colnames(y_test) <- "activityid"
colnames(subject_test) <- "subjectid"

xy_test <- bind_cols(subject_test, y_test) %>% bind_cols(x_test)

## Read train data set and bind the activity and subject id
x_train <- read.table(file = "./train/X_train.txt")
y_train <- read.table(file = "./train/y_train.txt")
subject_train <- read.table(file = "./train/subject_train.txt")
colnames(y_train) <- "activityid"
colnames(subject_train) <- "subjectid"

xy_train <- bind_cols(subject_train, y_train) %>% bind_cols(x_train)

## Bind both test and train data set
alldata <- bind_rows(xy_train, xy_test)


##
## STEP TWO: 
##
## Extracts only the measurements on the mean and standard
## deviation for each measurement. 
##

features <- read.table(file = "./features.txt", stringsAsFactors = FALSE)

colnames(features) <- c("measureid", "measure")

# Extract feature ids which is mean or std
selected_features <- features[c(grep("std\\(\\)", features$measure), grep("mean\\(\\)", features$measure)),]
featureid <- select(selected_features, measureid)

# Convert data frame column to vector by calling dtfm[[1]]
# The id in the data set is offset by 2 which are subjectid and activityid
# Extract the data (mean and std)
mean_std_measure <- alldata[,c(1, 2, featureid[[1]] + 2)]


##
##  STEP THREE:
##
##  Uses descriptive activity names to name the activities in the data set
##

activity_labels <- read.table(file = "./activity_labels.txt", stringsAsFactors = FALSE)

colnames(activity_labels) <- c("activityid", "activity")

selected_data <- merge(activity_labels, mean_std_measure, by = "activityid")


##
## STEP FOUR: 
##
## Appropriately labels the data set with descriptive variable names.
##

# The purpose I keep the underscore is it is easy for separate operation in tidyr
# which is very easy to separate the values in the data set
selected_features$measure <- gsub("-","_",selected_features$measure)
selected_features$measure <- gsub("std\\(\\)","StandardDeviation",selected_features$measure)
selected_features$measure <- gsub("mean\\(\\)","MeanValue",selected_features$measure)
selected_features$measure <- gsub("Acc","Accelerometer",selected_features$measure)
selected_features$measure <- gsub("Gyro","Gyroscope",selected_features$measure)
selected_features$measure <- gsub("BodyBody","Body",selected_features$measure)
selected_features$measure <- tolower(selected_features$measure)


# The first 3 column names are activityid, activity, subjectid
colnames(selected_data) <- c(colnames(selected_data)[1:3], selected_features$measure)

##
## STEP FIVE: 
##
## From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject.
##

# Use tidyr's "gather" function to transform the variables in original data set to 
# values in new column (measure)
# (Original data set size: 10299 * 69, new data set size: 679734 * 5)
res <- gather(selected_data, measure, value, -(activityid:subjectid))

# Summarize the dataset (tidy data set size: 11880 * 4)
tidydata <- summarize(group_by(res, activityid, subjectid, measure), meanvalue = mean(value))

write.table(tidydata, "tidydata.txt", row.names = FALSE)
