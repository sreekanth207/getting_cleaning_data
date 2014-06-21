##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
# runAnalysis.R

##########################################################################################################

# 1. Merge the training and the test sets to create one data set.

#set working directory 

setwd("C://Users/sreekantha/Documents/data-science/Assignments/sub3/feed/UCI HAR Dataset")
#getwd()

# Load in the data from files
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activity_labels = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subject_train = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
x_train       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
y_train       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt


# Add column names
colnames(activity_labels)  = c('activityId','activity_labels');
colnames(subject_train)  = "subjectId";
colnames(x_train)        = features[,2]; 
colnames(y_train)        = "activityId";

# Generate the final training set by merging y_train, subject_train, and x_train
trainingData = cbind(y_train,subject_train,x_train);

# Read in the test data
subject_test = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
x_test       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
y_test       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names
colnames(subject_test) = "subjectId";
colnames(x_test)       = features[,2]; 
colnames(y_test)       = "activityId";


# Generate the final test set by merging the x_test, y_test and subject_test data
testData = cbind(y_test,subject_test,x_test);


# Final data set by Combining training and test data
finalData = rbind(trainingData,testData);


# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];
#head(finalData,3)

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activity_labels,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;
#head(finalData,3)

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoactivity_labels without the activity_labels column
finalDataNoactivity_labels  = finalData[,names(finalData) != 'activity_labels'];

# Summarizing the finalDataNoactivity_labels table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoactivity_labels[,names(finalDataNoactivity_labels) != c('activityId','subjectId')],by=list(activityId=finalDataNoactivity_labels$activityId,subjectId = finalDataNoactivity_labels$subjectId),mean);

# Merging the tidyData with activity_labels to include descriptive acitvity names
tidyData    = merge(tidyData,activity_labels,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
