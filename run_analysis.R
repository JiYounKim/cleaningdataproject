
##0.read raw data from UCI Machine Learning Repository##

feature<-read.table("features.txt")

##Train DataSet ##

train<-read.table("X_train.txt",col.names=feature$V2)
train_activity<-read.table("y_train.txt")
train_subject<-read.table("subject_train.txt")
train_total_acc_x<-read.table("total_acc_x_train.txt")
train_total_acc_y<-read.table("total_acc_y_train.txt")
train_total_acc_z<-read.table("total_acc_z_train.txt")
train_body_acc_x<-read.table("body_acc_x_train.txt")
train_body_acc_y<-read.table("body_acc_y_train.txt")
train_body_acc_z<-read.table("body_acc_z_train.txt")
train_body_gyro_x<-read.table("body_gyro_x_train.txt")
train_body_gyro_y<-read.table("body_gyro_y_train.txt")
train_body_gyro_z<-read.table("body_gyro_z_train.txt")

##Test DataSet ##

test<-read.table("X_test.txt",col.names=feature$V2)
test_activity<-read.table("y_test.txt")
test_subject<-read.table("subject_test.txt")
test_total_acc_x<-read.table("total_acc_x_test.txt")
test_total_acc_y<-read.table("total_acc_y_test.txt")
test_total_acc_z<-read.table("total_acc_z_test.txt")
test_body_acc_x<-read.table("body_acc_x_test.txt")
test_body_acc_y<-read.table("body_acc_y_test.txt")
test_body_acc_z<-read.table("body_acc_z_test.txt")
test_body_gyro_x<-read.table("body_gyro_x_test.txt")
test_body_gyro_y<-read.table("body_gyro_y_test.txt")
test_body_gyro_z<-read.table("body_gyro_z_test.txt")



##1.Merge the training and the test sets to create one data set##

merge_x<-rbind(train,test)
merge_activity<-rbind(train_activity,test_activity)
merge_subject<-rbind(train_subject,test_subject)
merge_total_acc_x<-rbind(train_total_acc_x,test_total_acc_x)
merge_total_acc_y<-rbind(train_total_acc_y,test_total_acc_y)
merge_total_acc_z<-rbind(train_total_acc_z,test_total_acc_z)
merge_body_acc_x<-rbind(train_body_acc_x,test_body_acc_x)
merge_body_acc_y<-rbind(train_body_acc_y,test_body_acc_y)
merge_body_acc_z<-rbind(train_body_acc_z,test_body_acc_z)
merge_body_gyro_x<-rbind(train_body_gyro_x,test_body_gyro_x)
merge_body_gyro_y<-rbind(train_body_gyro_y,test_body_gyro_y)
merge_body_gyro_z<-rbind(train_body_gyro_z,test_body_gyro_z)

all<-data.frame(merge_subject,merge_x,merge_activity, 
               merge_total_acc_x,merge_total_acc_y,merge_total_acc_z,
               merge_body_acc_x,merge_body_acc_y,merge_body_acc_z,
               merge_body_gyro_x,merge_body_gyro_y,merge_body_gyro_z)

##2. Extract only the measurements on the##
##mean adn standard deviation for each measurement## 

mean_indicator<-grep("\\bmean\\b",feature$V2)
sd_indicator<-grep("std",feature$V2)
indi<-sort(c(mean_indicator,sd_indicator))

merge_x_sub<-merge_x[,indi]
all<-data.frame(merge_subject,merge_x_sub,merge_activity, 
                merge_total_acc_x,merge_total_acc_y,merge_total_acc_z,
                merge_body_acc_x,merge_body_acc_y,merge_body_acc_z,
                merge_body_gyro_x,merge_body_gyro_y,merge_body_gyro_z)

## 3.Uses descriptive activity names to name ##
##the activities in the data set ##


for (i in 1:nrow(all)){

if  (all$V1.1[i]==1) {all$V1.1[i]="WALKING"} 
   else if (all$V1.1[i]==2){all$V1.1[i]="WALKING_UPSTAIRS"} 
    else if (all$V1.1[i]==3){all$V1.1[i]="WALKING_DOWNSTAIR"}
      else if (all$V1.1[i]==4){all$V1.1[i]="SITTING"}
        else if (all$V1.1[i]==5){all$V1.1[i]="STANDING"}
          else {all$V1.1[i]="LAYING"}
}


## 4. Appropriately labes th data set with## 
##descriptive variable names.##

names(all)[1]<-"Subject"
names(all)[68]<-"Activity"


##create descriptive variable names## 

label_total_acc_x<-label_total_acc_y<-label_total_acc_z<-label_body_acc_x<-
  label_body_acc_y<-label_body_acc_z<-label_body_gyro_x<-label_body_gyro_y<-
  label_body_gyro_z<-NULL


for (i in 1:128){
  label_total_acc_x[i]<-paste("total_acc_x",i,collapse="")}
for (i in 1:128){
  label_total_acc_y[i]<-paste("total_acc_y",i,collapse="")}
for (i in 1:128){
  label_total_acc_z[i]<-paste("total_acc_z",i,collapse="")}

for (i in 1:128){
  label_body_acc_x[i]<-paste("body_acc_x",i,collapse="")}
for (i in 1:128){
  label_body_acc_y[i]<-paste("body_acc_y",i,collapse="")}
for (i in 1:128){
  label_body_acc_z[i]<-paste("body_acc_z",i,collapse="")}

for (i in 1:128){
  label_body_gyro_x[i]<-paste("body_gyro_x",i,collapse="")}
for (i in 1:128){
  label_body_gyro_y[i]<-paste("body_gyro_y",i,collapse="")}
for (i in 1:128){
  label_body_gyro_z[i]<-paste("body_gyro_z",i,collapse="")}


names(all)[69:ncol(all)]<-c(label_total_acc_x,label_total_acc_y,label_total_acc_z,
                            label_body_acc_x,label_body_acc_y,label_body_acc_z,
                            label_body_gyro_x,label_body_gyro_y,label_body_gyro_z)


##5.From the data set in step 4. creates a second, independent tidy data set##
##with the average of each varaible for each activity and each subject##


newindi<-grep("mean",names(all))

newindi<-c(1,newindi,68)

final<-all[,newindi]
write.table(final,"finaltidydataset.txt",row.name=FALSE)
