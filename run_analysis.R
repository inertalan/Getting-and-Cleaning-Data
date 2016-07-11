library(data.table)
library(dplyr)

#1.Merges the training and the test sets to create one data set.
##Get data form uel
daturl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- paste0(getwd(),"/dat.zip")
download.file(daturl,destfile = f)
unzip(f)


##Read data and merge
test <- paste0(getwd(),"/UCI HAR Dataset/test/subject_test.txt")%>%read.table()
train <- paste0(getwd(),"/UCI HAR Dataset/train/subject_train.txt")%>%read.table()
subject <- rbind(test,train)
subject <- rename(.data = subject, subject = V1)

test_y <- paste0(getwd(),"/UCI HAR Dataset/test/y_test.txt")%>%read.table()
train_y <- paste0(getwd(),"/UCI HAR Dataset/train/y_train.txt")%>%read.table()
activity <- rbind(test_y,train_y)
activity <- rename(.data = activity, activity = V1)

test_x <- paste0(getwd(),"/UCI HAR Dataset/test/X_test.txt")%>%read.table()
train_x <- paste0(getwd(),"/UCI HAR Dataset/train/X_train.txt")%>%read.table()
dt <- rbind(test_x,train_x)
dt <- cbind(subject,activity)%>%cbind(dt)
dim(dt)#10299*563

#2.Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table(paste0(getwd(),"/UCI HAR Dataset/features.txt"))
features <- mutate(features, nvar = paste0("V",V1))%>%select(-V1)
features <- features[grepl(pattern = "mean\\(\\)|std\\(\\)",x = features$V2),] #subset variables contains mean and sd
use <- as.vector(features[,2])#generate indicators of needed variables
dt <- dt[,c("activity","subject",use)]
dim(dt)
#10299  68

#3.Uses descriptive activity names to name the activities in the data set

##Read activity names from activity_label.txt, and name activities in data set
actlabel <- read.table(paste0(getwd(),"/UCI HAR Dataset/activity_labels.txt"))
actlabel <- rename(actlabel, activity = V1, activities = V2)
dt <- merge(dt, actlabel, by= "activity")
dt <- dt[,-1]
dt$subject <- as.factor(dt$subject)
#I have to grap the activity variable to the front, 
#But I can not think of any other method...
dt <- select(dt,activities,subject:V543)
vname <- features[,1]
vname <- gsub(pattern = "()",replacement = "",x = vname,fixed = T)
#Rename
names(dt)[3:68] <- vname

str(dt)
#'data.frame':	10299 obs. of  68 variables:
#$ activities               : Factor w/ 6 levels "LAYING","SITTING",..: 4 4 4 4 4 4 4 4 4 4 ...
#$ subject                  : Factor w/ 30 levels "1","2","3","4",..: 7 21 7 7 18 7 7 7 11 21 ...
#$ tBodyAcc-mean-X          : num  0.269 0.262 0.238 0.245 0.249 ...
#$ tBodyAcc-mean-Y          : num  0.00789 -0.01622 0.0021 -0.03155 -0.02112 ...
#$ tBodyAcc-mean-Z          : num  -0.0507 -0.1145 -0.0499 -0.1814 -0.1249 ...
#$ tBodyAcc-std-X           : num  -0.264 -0.415 -0.338 -0.311 -0.491 ...
#$ tBodyAcc-std-Y           : num  0.0324 0.1461 0.0563 0.0977 -0.1977 ...
#$ tBodyAcc-std-Z           : num  0.10128 -0.10997 0.00291 0.09829 -0.58428 ...
#......



tidydat <- aggregate(x=dt, by=list(activities=dt$activities, subj=dt$subject), FUN="mean")
tidydat <- tidydat[,-c(3,4)]
dim(tidydat)
#180  68



