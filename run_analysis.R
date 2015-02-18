## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Uses descriptive activity names to name the activities in the data sets
features_names <- read.table("./UCI HAR Dataset/features.txt")[,2]

## Read the test data
labels_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "labels")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subjects")
## Extracts only the measurements on the mean and standard deviation for each measurement
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features_names)[,c(1:6, 41:46, 81:86, 121:126,161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)]
## bind the labels, subjects and test data together
x_test <- cbind(labels_test, subject_test, x_test)

## Read the train data
labels_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "labels")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subjects")
## Extracts only the measurements on the mean and standard deviation for each measurement
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features_names)[,c(1:6, 41:46, 81:86, 121:126,161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)]
## bind the labels, subjects and test data together
x_train <- cbind(labels_train, subject_train, x_train)

## Merges the training and the test sets to create one data set.
x_total <- rbind(x_test,x_train)

## Subjects and Labels ordered as in the result of the melt
x_total_mean <- data.frame(subjects = rep(1:30, times=6), labels = rep(1:6, each=30))

## creates a data set with the average of each variable for each activity and each subject.
for (n in 3:ncol(x_total)) {
    ## melt every variable of x on it's mean by subject and labels 
    x_mean <- tapply(x_total[,n], c(x_total["subjects"],x_total["labels"]), mean)
    x_mean <- melt(x_mean, measure.vars = c(1:6), value.name=names(x_total)[n])
    ## bind the mean of the measurement together with the other means
    x_total_mean <- cbind(x_total_mean, x_mean[3])
}

## Returns the final data.frame
x_total_mean
