library(dplyr)
library(data.table)

#Setting File
get_file <-  "Coursera_DS3_Final.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

directory <- getwd()
#checking the file and downloading it

if (!file.exists(get_file)){
  download.file(url, get_file, method="curl")
}  

#unzipping file
unzip(zipfile = get_file)

#loading the files and getting the data

activity_labels <- fread(file.path(directory,"UCI HAR Dataset/features.txt"),
                         col.names = c("classLabels","activityName"))


features <- fread(file.path(directory, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))


train <- fread(file.path(directory, "UCI HAR Dataset/train/X_train.txt"),colClasses = features$featureNames)


trainActivities <- fread(file.path(directory, "UCI HAR Dataset/train/Y_train.txt"), col.names = c("Activity"))


trainSubjects <- fread(file.path(directory, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))


test <- fread(file.path(directory,"UCI HAR Dataset/test/X_test.txt"), colClasses = features$featureNames)


testActivities <- fread(file.path(directory, "UCI HAR Dataset/test/Y_test.txt"),col.names = c("Activity"))

testSubjects <- fread(file.path(directory, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))


#merging the data sets
subjects <- cbind(trainSubjects,testSubjects)
train <- cbind(trainSubjects,trainActivities,train)
test <- cbind(testSubjects,testActivities,test)

merge_data <- rbind(train,test)

#Extracts only the measurements on the mean and standard deviation for each measurement.

cal_measurement <- merge_data %>% select(merge_data$SubjectNum,contains("mean"),contains("std"))

#Uses descriptive activity names to name the activities in the data set

cal_measurement$index <- activity_labels[cal_measurement$index,2]

#appropriately labels the data set with descriptive variable names

names(cal_measurement)[2] = "Activity"
names(cal_measurement)<-gsub("Acc", "Accelerometer", names(cal_measurement))
names(cal_measurement)<-gsub("Gyro", "Gyroscope", names(cal_measurement))
names(cal_measurement)<-gsub("BodyBody", "Body", names(cal_measurement))
names(cal_measurement)<-gsub("Mag", "Magnitude", names(cal_measurement))
names(cal_measurement)<-gsub("^t", "Time", names(cal_measurement))
names(cal_measurement)<-gsub("^f", "Frequency", names(cal_measurement))
names(cal_measurement)<-gsub("tBody", "TimeBody", names(cal_measurement))
names(cal_measurement)<-gsub("-mean()", "Mean", names(cal_measurement), ignore.case = TRUE)
names(cal_measurement)<-gsub("-std()", "STD", names(cal_measurement), ignore.case = TRUE)
names(cal_measurement)<-gsub("-freq()", "Frequency", names(cal_measurement), ignore.case = TRUE)
names(cal_measurement)<-gsub("angle", "Angle", names(cal_measurement))
names(cal_measurement)<-gsub("gravity", "Gravity", names(cal_measurement))


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- cal_measurement %>%
  group_by(SubjectNum, Activity) %>%
  summarise_all(funs(mean))

write.table(FinalData, "FinalData.txt", row.name=FALSE)

