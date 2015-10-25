# CodeBook
## Description of variables, data and transformations performed

The original data for this data analysis project can be found in the following link (as described in the README):

[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
 
The original data was separated into training and test files, which in turn referenced features, activities and subjects detailed in separate specific files.

### Transformations performed

1.	Mean and Deviation (mean and std) were extracted from training and test data into 2 separated data frames.
2.	The columns of these data frames were properly named.
3.	A new column SubjectID was added in both data frames, populating it according to \subject_train.txt
4.	A new column Activity was added in both data frames, populating it according to \y_train.txt and activity_labels.txt
5.	Both data frames were merged into one data frame
6.	A new data frame was created with the average of each variable for each activity and each subject.

### Variables

The first two variables are:

*	**activity** - 'WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING' or 'LAYING'
*	**subject** - The code indicating which volunteer performed the activity

For this project, only *mean* and *std (standard deviation)* attributes were relevant, therefore, using the same original descriptions, the set of variables used for calculating the average for each activity and subject is as follows:


> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
> 
> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
> 
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
> 
> These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

*	tBodyAcc-XYZ
*	tGravityAcc-XYZ
*	tBodyAccJerk-XYZ
*	tBodyGyro-XYZ
*	tBodyGyroJerk-XYZ
*	tBodyAccMag
*	tGravityAccMag
*	tBodyAccJerkMag
*	tBodyGyroMag
*	tBodyGyroJerkMag
*	fBodyAcc-XYZ
*	fBodyAccJerk-XYZ
*	fBodyGyro-XYZ
*	fBodyAccMag
*	fBodyAccJerkMag
*	fBodyGyroMag
*	fBodyGyroJerkMag
 
> The set of variables that were estimated from these signals are: 
 
*	Mean: Mean value
*	Std: Standard deviation
 