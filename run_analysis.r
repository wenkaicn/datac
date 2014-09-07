## Merges the training and the test sets to create one data set.

if(!file.exists("data.txt")) {
	file.create("data.txt")

	dataTrain <- read.table(".\\train\\X_train.txt")
	dataTest <- read.table(".\\test\\X_test.txt")
	dataTotal <- rbind(dataTrain, dataTest)
	write.table(dataTotal,"data.txt")
} else {
	dataTotal <- read.table("data.txt")
}


## Extracts only the measurements on the mean and standard deviation for each measurement.


features <- read.table("features.txt")
desVar <- vector("character")

for (i in seq_len(nrow(features))){
	#print(features[i,2]) 
	if (agrepl("mean()",features[i,2]) || agrepl("std()",features[i,2])){
		desVar <- append(desVar, as.character(features[i,2]))
	} else {
		features[i,2] = NA
	}
	bad <- is.na(features[[2]])
	d <- dataTotal[!bad]
}

## Uses descriptive activity names to name the activities in the data set

labels<-read.table("activity_labels.txt")

activityTrain <- read.table(".\\train\\y_train.txt")
activityTest <- read.table(".\\test\\y_test.txt")
activityTotal <- rbind(activityTrain , activityTest )
activityTotalA <- activityTotal[[1]]

for (i in seq_len(nrow(activityTotal))){
	activityTotalA[i] <- as.character(labels[activityTotalA[i],2])
}

activity <- factor(activityTotalA,
  levels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING","LAYING"))

d = data.frame(d, activity)

desVar <- append(desVar, "activity")

## Appropriately labels the data set with descriptive variable names. 

names(d) <- desVar 

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

a <- read.table(".\\train\\subject_train.txt")
b <- read.table(".\\test\\subject_test.txt")
subject <- rbind(a,b)

d = data.frame(d,subject)
desVar <- append(desVar, "subject")
names(d) <- desVar
aggdata <- aggregate(d, by=list(d$subject, d$activity), FUN=mean)
write.table(aggdata, "tidyData.txt", row.name = FALSE)

