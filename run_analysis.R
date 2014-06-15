##peer assignment
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%HAR%20Dataset.zip"
download.file(fileurl,destfile="peerassignment.zip", method="curl")

## Unzip file to extract the following files to the working directory:
##     X_test.txt, y_test.txt, X_train.txt, y_train.txt, features.txt
library(plyr)
## process test and train data separately
subject_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("Y_test.txt")
vnames <- read.table("features.txt")
# group = 1 for test, group=2 for train
#subject_test["group"] <- 1
names(subject_test) <- c("subject")
names(y_test) <- c("activity")

## create an index variable to use to merge the subject id and activity id files
#s1 <- seq(1,2947)
s1 <- seq(1, nrow(subject_test))
subject_test["index"] <- s1
y_test["index"] <- s1
test1 <- merge(subject_test, y_test, by.x="index", by.y="index")

## select variables on the mean and std for each measurement
#meanstd is a dataframe of the selected variables
meanstd <- vnames[grep("mean|Mean|std", vnames$V2),]
usecol <- meanstd$V1
x_test1 <- x_test[usecol]
x_test1["index"] <- s1
test_data <- merge(test1, x_test1, by="index")


## process train data
subject_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("Y_train.txt")
vnames <- read.table("features.txt")
# group = 1 for test, group=2 for train
#subject_train["group"] <- 2
names(subject_train) <- c("subject")
names(y_train) <- c("activity")

## create an index variable to use to merge the subject id and activity id files
s1 <- seq(1,nrow(subject_train))
subject_train["index"] <- s1
y_train["index"] <- s1
train1 <- merge(subject_train, y_train, by.x="index", by.y="index")

## use the selected variables on the mean and std for each measurement 
## meanstd (previously created) 
## meanstd <- vnames[grep("mean|Mean|std", vnames$V2),]
## columns were selected by using the row index (vnames$V1)
usecol <- meanstd$V1
x_train1 <- x_train[usecol]
x_train1["index"] <- s1
train_data <- merge(train1, x_train1, by="index")

## append train data to test data
combined <- rbind(test_data, train_data)

## assign names to the measurement variables
for (i in (1:nrow(meandstd))) {     
        x = paste("V",meanstd[i,1], sep="")
        meanstd[i,1]=x
        print(x)
}
oldnames <- as.vector(meanstd$V1)
newnames <- as.vector(meanstd$V2)
names(combined)[match(oldnames,names(combined))] <- newnames

## assign value labels to activity
combined$activity <- factor(combined$activity, levels = c(1,2,3,4,5,6),
                            labels=c("walking","walking upstairs","walking downstairs",
                                     "sitting","standing","laying"))

### first data set created.

## next create the second data set with the mean of each variable for each activity
## and each subject
library(plyr)
combined$index <- NULL
combined$group <- NULL
avg_measurements <- ddply(combined,c("subject","activity"),
                          function(combined) colMeans(combined))

## save data sets to disk
write.csv(file="combined.csv", x=combined)
write.csv(file="average_measurements.csv", x=avg_measurements)
