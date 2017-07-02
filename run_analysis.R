#
setwd("./Coursera - Data Science Specialization/03. Cleaning Data/UCI HAR Dataset")
# Read the traininng data from files
features     = read.table('./features.txt',header=FALSE) 
activitylbl = read.table('./activity_labels.txt',header=FALSE) 
subject_train = read.table('./train/subject_train.txt',header=FALSE)
x_train       = read.table('./train/x_train.txt',header=FALSE) 
y_train       = read.table('./train/y_train.txt',header=FALSE) 

# Assigin column names to the data imported above
colnames(activitylbl)  = c('activityId','activityType')
colnames(subject_train)  = "subjectId"
colnames(x_train)        = features[,2] 
colnames(y_train)        = "activityId"

# Create the final training set
trainingData = cbind(y_train,subject_train,x_train)

# Read in the test data
subject_test = read.table('./test/subject_test.txt',header=FALSE)
x_test       = read.table('./test/x_test.txt',header=FALSE) 
y_test       = read.table('./test/y_test.txt',header=FALSE) 

# Assign column names to the test data imported above
colnames(subject_test) = "subjectId"
colnames(x_test)       = features[,2] 
colnames(y_test)       = "activityId"


# Create the final test set
testData = cbind(y_test,subject_test,x_test)


# Combine training and test data to create a merged data set
MergedData = rbind(trainingData,testData)

# Create a vector for the column names from the MergedData, which will be used
# to select the desired mean() & stddev() columns
column_names  = colnames(MergedData) 


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",column_names) | grepl("subject..",column_names) | grepl("-mean..",column_names) & !grepl("-meanFreq..",column_names) & !grepl("mean..-",column_names) | grepl("-std..",column_names) & !grepl("-std()..-",column_names));

# Subset MergedData table based on the logicalVector to keep only desired columns
MergedData = MergedData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the MergedData set with the acitivityType table to include descriptive activity names
MergedData = merge(MergedData,activitylbl,by='activityId',all.x=TRUE);

# Updating the column_names vector to include the new column names after merge
column_names  = colnames(MergedData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(column_names)) 
{
        column_names[i] = gsub("\\()","",column_names[i])
        column_names[i] = gsub("-std$","StdDev",column_names[i])
        column_names[i] = gsub("-mean","Mean",column_names[i])
        column_names[i] = gsub("^(t)","time",column_names[i])
        column_names[i] = gsub("^(f)","freq",column_names[i])
        column_names[i] = gsub("([Gg]ravity)","Gravity",column_names[i])
        column_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",column_names[i])
        column_names[i] = gsub("[Gg]yro","Gyro",column_names[i])
        column_names[i] = gsub("AccMag","AccMagnitude",column_names[i])
        column_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",column_names[i])
        column_names[i] = gsub("JerkMag","JerkMagnitude",column_names[i])
        column_names[i] = gsub("GyroMag","GyroMagnitude",column_names[i])
};

# Reassigning the new descriptive column names to the MergedData set
colnames(MergedData) = column_names;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, MergedDataNoActivityType without the activityType column
MergedDataNoActivityType  = MergedData[,names(MergedData) != 'activityType'];

# Summarizing the MergedDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(MergedDataNoActivityType[,names(MergedDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=MergedDataNoActivityType$activityId,subjectId = MergedDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activitylbl,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')
