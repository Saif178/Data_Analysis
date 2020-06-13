# creating Training dataset
X_train<-readLines('./UCI HAR Dataset/train/X_train.txt')
X_train<-lapply(X_train,trimws)
X_train<-lapply(X_train,strsplit,' ')
for(i in 1:length(X_train)){X_train[[i]][[1]]<-as.numeric(X_train[[i]][[1]])}
x<-X_train[[1]][[1]][!is.na(X_train[[1]][[1]])]
x<-rbind(x,X_train[[2]][[1]][!is.na(X_train[[2]][[1]])])
x<-as.data.frame(x)
for(i in 3:length(X_train)){x<-rbind(x,X_train[[i]][[1]][!is.na(X_train[[i]][[1]])])}
row.names(x)<-1:length(X_train)
features<-readLines('./UCI HAR Dataset/features.txt')
features[1:9]<-gsub('\\d','',features[1:9])
features[10:99]<-gsub('\\d{2}','',features[10:99])
features[100:561]<-gsub('\\d{3}','',features[100:561])
features<-trimws(features)
features<-gsub('\\(\\)','',features)
colnames(x)<-features
y_train<-readLines('./UCI HAR Dataset/train/y_train.txt')
y_train<-as.numeric(y_train)
x<-cbind(x,y_train)

# creating Testing dataset
X_test<-readLines('./UCI HAR Dataset/test/X_test.txt')
X_test<-lapply(X_test,trimws)
X_test<-lapply(X_test,strsplit,' ')
for(i in 1:length(X_test)){X_test[[i]][[1]]<-as.numeric(X_test[[i]][[1]])}
x1<-X_test[[1]][[1]][!is.na(X_test[[1]][[1]])]
x1<-rbind(x1,X_test[[2]][[1]][!is.na(X_test[[2]][[1]])])
x1<-as.data.frame(x1)
for(i in 3:length(X_test)){x1<-rbind(x1,X_test[[i]][[1]][!is.na(X_test[[i]][[1]])])}
row.names(x1)<-1:length(X_test)
colnames(x1)<-features
y_test<-readLines('./UCI HAR Dataset/test/y_test.txt')
y_test<-as.numeric(y_test)
x1<-cbind(x1,y_test)

#creating complete dataset
colnames(x)[562]<-'activitylabel'
colnames(x1)[562]<-'activitylabel'
y<-rbind(x,x1)
sub_train<-readLines('./UCI HAR Dataset/train/subject_train.txt')
sub_test<-readLines('./UCI HAR Dataset/test/subject_test.txt')
subject<-c(sub_train,sub_test)
subject<-as.numeric(subject)
y<-cbind(y,subject)

#tidying data
activity<-readLines('./UCI HAR Dataset/activity_labels.txt')
activity<-gsub('\\d','',activity)
activity<-trimws(activity)
z<-y[grepl('mean|std',names(y))]
p<-z[!grepl('meanFreq',names(z))]
p$subject<-y$subject
p$activitylabel<-y$activitylabel
p$activitylabel<-activity[p$activitylabel]
q<-group_by(p,subject,activitylabel)
r<-summarise(q,mean(q$`tBodyAcc-mean-X`),mean(q$`tBodyAcc-mean-Y`),
             mean(q$`tBodyAcc-mean-Z`),mean(q$`tBodyAcc-std-X`),
             mean(q$`tBodyAcc-std-Y`),mean(q$`tBodyAcc-std-Z`),
             mean(q$`tGravityAcc-mean-X`),mean(q$`tGravityAcc-mean-Y`),
             mean(q$`tGravityAcc-mean-Z`),mean(q$`tGravityAcc-std-X`),
             mean(q$`tGravityAcc-std-Y`),mean(q$`tGravityAcc-std-Z`),
             mean(q$`tBodyAccJerk-mean-X`),mean(q$`tBodyAccJerk-mean-Y`),
             mean(q$`tBodyAccJerk-mean-Z`),mean(q$`tBodyAccJerk-std-X`),
             mean(q$`tBodyAccJerk-std-Y`),mean(q$`tBodyAccJerk-std-Z`),
             mean(q$`tBodyGyro-mean-X`),mean(q$`tBodyGyro-mean-Y`),
             mean(q$`tBodyGyro-mean-Z`),mean(q$`tBodyGyro-std-X`),
             mean(q$`tBodyGyro-std-Y`),mean(q$`tBodyGyro-std-Z`),
             mean(q$`tBodyGyroJerk-mean-X`),mean(q$`tBodyGyroJerk-mean-Y`),
             mean(q$`tBodyAccJerk-mean-Z`),mean(q$`tBodyGyroJerk-std-X`),
             mean(q$`tBodyGyroJerk-std-Y`),mean(q$`tBodyGyroJerk-std-Z`),
             mean(q$`tBodyAccMag-mean`),mean(q$`tBodyAccMag-std`),
             mean(q$`tBodyAccJerkMag-mean`),mean(q$`tBodyAccJerkMag-std`),
             mean(q$`tBodyGyroMag-mean`),mean(q$`tBodyGyroMag-std`),
             mean(q$`tBodyGyroJerkMag-mean`),mean(q$`tBodyGyroJerkMag-std`),
             mean(q$`fBodyAcc-mean-X`),mean(q$`fBodyAcc-mean-Y`),
             mean(q$`fBodyAcc-mean-Z`),mean(q$`fBodyAcc-std-X`),
             mean(q$`fBodyAcc-std-Y`),mean(q$`fBodyAcc-std-Z`),
             mean(q$`fBodyAccJerk-mean-X`),mean(q$`fBodyAccJerk-mean-Y`),
             mean(q$`fBodyAccJerk-std-Z`),mean(q$`fBodyGyro-mean-X`),
             mean(q$`fBodyGyro-mean-Y`),mean(q$`fBodyGyro-mean-Z`),
             mean(q$`fBodyGyro-std-X`),mean(q$`fBodyGyro-std-Y`),
             mean(q$`fBodyGyro-std-Z`),mean(q$`fBodyAccMag-mean`),
             mean(q$`fBodyAccMag-std`),mean(q$`fBodyBodyAccJerkMag-mean`),
             mean(q$`fBodyBodyAccJerkMag-std`),mean(q$`fBodyBodyGyroMag-mean`),
             mean(q$`fBodyBodyGyroMag-std`),mean(q$`fBodyBodyGyroJerkMag-mean`),
             mean(q$`fBodyBodyGyroJerkMag-std`))
features_1<-features[grepl('mean|std',features)]
features_1<-features_1[!grepl('meanFreq',features_1)]
names(r)[3:62]<-features_1
#writing data
write.table(r,file='analysis.txt',row.names=FALSE)