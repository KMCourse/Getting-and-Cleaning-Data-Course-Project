#=============================
#  Subject: Getting and Cleaning Data Course Project
#  Author : KMCourse
#  Data   : 09/03/2017 
#=============================

#change the workload folder
    #setwd("C:/Users/Me/Dropbox/Lessons/Data Science/Getting and Cleaning Data/Project")
#Check the workload path    
getwd()

#Download project data set.
dataset.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset.url, "Dataset.zip")

#Unzip the file
unzip("Dataset.zip", exdir = "dataset")

# ============================================================================
# 1.Merges the training and the test sets to create one data set.
# ============================================================================

#load train data
x.train <- read.table("./dataset/UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("./dataset/UCI HAR Dataset/train/y_train.txt")
subject.train <- read.table("./dataset/UCI HAR Dataset/train/subject_train.txt")

#load test data
x.test <- read.table("./dataset/UCI HAR Dataset/test/X_test.txt")
y.test <- read.table("./dataset/UCI HAR Dataset/test/y_test.txt")
subject.test <- read.table("./dataset/UCI HAR Dataset/test/subject_test.txt")

# virw columns names
colnames(x.train)


# Change columns names to be readable and same in all objects

# load features
features <- read.table('./dataset/UCI HAR Dataset/features.txt')
# load activity labels
activity.Labels = read.table('./dataset/UCI HAR Dataset/activity_labels.txt')


colnames(x.train) <- features[, 2]
colnames(y.train) <- "activityId"
colnames(subject.train) <- "subjectId"

colnames(x.test) <- features[, 2]
colnames(y.test) <- "activityId"
colnames(subject.test) <- "subjectId"

colnames(activity.Labels) <- c('activityId', 'activityType')

# virw columns names
colnames(x.train)

# Mearge all data objects to one object

#Combine all columns in the training
train.data <- cbind(x.train, y.train, subject.train)

#Combine all columns in the test
test.data <- cbind(x.test, y.test, subject.test)

#Mearge the data from training and test
sgs.data <- rbind(train.data, test.data)

summary(sgs.data)

#=============================================================================================
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#=============================================================================================

cloumn.names <- colnames(sgs.data)

mean.std <- (grepl("activityId", cloumn.names) |
                 grepl("subjectId", cloumn.names) |
                 grepl("mean..", cloumn.names) |
                 grepl("std..", cloumn.names)
)

mean.Std <- sgs.data[, mean.std == TRUE]

#==============================================================================================
# 3.Using descriptive activity names to name the activities in the data set
#==============================================================================================

activity.names <- merge(mean.Std, activity.Labels,
                              by = 'activityId',
                              all.x = TRUE)


#==============================================================================================
# 4.Appropriately labels the data set with descriptive variable names
#==============================================================================================

names(mean.Std) <- gsub("^t", "time", names(mean.Std))
names(mean.Std) <- gsub("^f", "frequency", names(mean.Std))
names(mean.Std) <- gsub("Acc", "Accelerometer", names(mean.Std))
names(mean.Std) <- gsub("Gyro", "Gyroscope", names(mean.Std))
names(mean.Std) <- gsub("Mag", "Magnitude", names(mean.Std))
names(mean.Std) <- gsub("BodyBody", "Body", names(mean.Std))

names(mean.Std)

#==============================================================================================
# 4.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
#==============================================================================================

tidy.set <- aggregate(. ~ subjectId + activityId, setWithActivityNames, mean)
tidy.set <- tidy.set[order(tidy.set$subjectId, tidy.set$activityId),]

#=============================================================
write.table(secTidySet, "TidyDataSet.txt", row.name = FALSE)
