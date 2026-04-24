#download data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "dataset.zip")

#unzip file and create the foldes "UCI HAR Dataset
unzip("dataset.zip", overwrite = F)

library(tidyverse)
list.files("UCI HAR Dataset")
list.files("UCI HAR Dataset/test")

x_test<- read.table("UCI HAR Dataset/test/X_test.txt") #read test set
x_train<- read.table("UCI HAR Dataset/train/X_train.txt") #read train set


sub_test<- read.table("UCI HAR Dataset/test/subject_test.txt") #column containing the subject ID
sub_train<- read.table("UCI HAR Dataset/train/subject_train.txt")#column containing the subject ID

y_test<- read.table("UCI HAR Dataset/test/y_test.txt") #read activity labels
y_train<- read.table("UCI HAR Dataset/train/y_train.txt") #read activity labels

lab<- read.table("UCI HAR Dataset/features.txt") #dataset containing the labels
colnames(x_test)<- lab[,2] #assign the name to the variables
colnames(x_train)<- lab[,2] #assign the name to the variables

### now we hato to add the column containing the activity and subject label to
### x_test and x_train
colnames(sub_test) <- "Subject"  #change the names of the columns 
colnames(sub_train) <- "Subject"
colnames(y_test) <- "Activity"
colnames(y_train) <- "Activity"

x_test<- bind_cols(x_test, y_test, sub_test)
x_train<- bind_cols(x_train, y_train, sub_train)

### create a new dataset containing both train and test data
df<- bind_rows(x_train, x_test) #merge the two dataframes into a "longer" one

df <- df %>% #change the name of the activity according to the file activity_labels.txt
  mutate(Activity = case_when(
    Activity == 1 ~ "WALKING",
    Activity == 2 ~ "WALKING_UPSTAIRS",
    Activity == 3 ~ "WALKING_DOWNSTAIRS",
    Activity == 4 ~ "SITTING",
    Activity == 5 ~ "STANDING",
    Activity == 6 ~ "LAYING"
  )) 



df_subset <- df %>% #extract the columns on mean and standard deviation
  select(Subject, Activity, contains("mean()"), contains("std()"))

#rename the columns the make them more readable
df_subset <- df_subset %>% 
  rename_with(~ gsub("^t", "Time_", .x)) %>% 
  rename_with(~ gsub("^f", "Frequency_", .x)) %>% 
  rename_with(~ gsub("\\(\\)", "", .x)) 

### create a tidy dataset containing the average of each variable for
### combination of activity and subject

df_final<- df_subset %>% 
  group_by(Activity, Subject) %>% 
  summarize(across(everything(), mean))    

###save the datasets in the folder
write.table(df_subset,  "tidy_dataset.txt", row.names = F)
write.table(df_final,  "tidy_average SubjectAactivity.txt", row.names = F)
