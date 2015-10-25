
f <- read.table("features.txt")

# 1- extracts 'mean' + 'std' 
meanMatches <- grep("*mean\\(", f[,2])
deviationMatches <- grep("*std\\(", f[,2])
matches <- c(meanMatches, deviationMatches)

# 2- name columns
xtrain <- read.table("train\\X_train.txt")
trainInfo <- xtrain[,matches]
colnames(trainInfo) <- as.character(f[matches,2])

xtest <- read.table("test\\X_test.txt")
testInfo <- xtest[,matches]
colnames(testInfo) <- as.character(f[matches,2])

# 3- add a new column to keep the subject ID 
trainSubject <- read.table("train\\subject_train.txt")
colnames(trainSubject) <- c("SubjectID")
trainInfo[ncol(trainInfo) + 1] <- trainSubject

testSubject <- read.table("test\\subject_test.txt")
colnames(testSubject) <- c("SubjectID")
testInfo[ncol(testInfo) + 1] <- testSubject

# 4- add a new column to keep the activity
activities <- read.table("activity_labels.txt")

ytrain <- read.table("train\\y_train.txt")
ytrain[ncol(ytrain) + 1] <- ""
colnames(ytrain) <- c("ActivityID", "Activity")

for (i in 1:nrow(ytrain)){
  ytrain[i,2] <- activities[ytrain[i,1] == activities[1]][2]
}

trainInfo[ncol(trainInfo) + 1] <- ytrain$Activity
colnames(trainInfo) <- c(colnames(trainInfo)[1:67], "Activity")

ytest <- read.table("test\\y_test.txt")
ytest[ncol(ytest) + 1] <- ""
colnames(ytest) <- c("ActivityID", "Activity")

for (i in 1:nrow(ytest)){
  ytest[i,2] <- activities[ytest[i,1] == activities[1]][2]
}

testInfo[ncol(testInfo) + 1] <- ytest$Activity
colnames(testInfo) <- c(colnames(testInfo)[1:67], "Activity")

# 5- merge train and test data sets through rbind 
# since they have the same columns but different amount of rows
myData <- rbind(trainInfo, testInfo)

# 6- create a new dataframe with two columns and 66 rows
#    * each cell will have the average for that column and row
#      - columns: subjectId and activity
#      - row: 66 variables
#     e.g. row 1 is going to have the average for 'tBodyAcc-mean()-X' 
#          by SubjectID (column 1) and by Activity (column 2)

# the avarage by SubjectID and Activity are going to be represented in a column
df = data.frame(SubjectID = (1:66), Activity = (1:66))
# each line is going to be a variable
rownames(df) <- colnames(myData)[1:66]

for (subject in 1:30){
  for (variable in 1:66){
    # column 67 keeps the SubjectId
    subjectMatches <- myData[,67] == subject
    myFilteredData <- myData[subjectMatches, ]
    
    values <- as.vector(myFilteredData[variable])
    numbers <- values[,1]
    # first column of df keeps the mean by Subject
    df[variable,1] <- mean(numbers)
  }
}

activities <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

for (activity in activities){
  for (variable in 1:66){
    # column 68 keeps the Activities
    activitiesMatches <- myData[,68] == activity
    myFilteredData <- myData[activitiesMatches, ]
    
    values <- as.vector(myFilteredData[variable])
    numbers <- values[,1]
    # second column of df keeps the mean by Activities
    df[variable,2] <- mean(numbers)
  }
}
