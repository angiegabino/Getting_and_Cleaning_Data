library(dplyr)

file1  <- "C:/Users/angel/Desktop/R/Course_Project/File_Project.zip"

# If file doesn't exist download
if (!file.exists(file1)){
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, file1, method="curl")
}  

if (!file.exists("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset")) { 
  unzip(file1) 
}

# Creating dataframes
features <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("C:/Users/angel/Desktop/R/Course_Project/UCI HAR Dataset/train/y_train.txt", col.names = "code")


#Step 1: Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
sub <- rbind(subject_train, subject_test)
datamerge <- cbind(sub, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

tidydata <- datamerge %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set.

tidydata$code <- activities[tidydata$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.

names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

finaldata <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(finaldata, "C:/Users/angel/Desktop/R/Course_Project/FinalData.txt", row.name=FALSE)
