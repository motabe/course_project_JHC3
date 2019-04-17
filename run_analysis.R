#
# alt + o to close all the sections
#
diretorio <- file.path(
  getwd(),
  "google-drive","documents","coursera","Johns Hopkins",
  "Course 3, Getting and Cleaning Data","R","week 4", "course project","github"
)
setwd(diretorio)
getwd()

# General Info Section ----------------------------------------------------
#
# Reading files, info files
#
raiz              <- file.path("UCI HAR Dataset")
# 'activity_labels.txt': Links the class labels with their activity name
activity_labels   = read.table(file.path(raiz, "activity_labels.txt"), header = FALSE)
# 'features.txt': List of all features
features          = read.table(file.path(raiz, "features.txt"), header = FALSE)

# brief description
# dim(activity_labels)  # activities (6) x (codes & labels) (2)
# dim(features)         # features (561) x (codes & labels) (2)

# Test Section ------------------------------------------------------------
#
# Reading files, test data
#
raiz              <- file.path("UCI HAR Dataset", "test")

# 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample
test_subject      = read.table(file.path(raiz, "subject_test.txt"), header = FALSE)
# 'test/X_test.txt': Test set
test_set          = read.table(file.path(raiz, "X_test.txt"), header = FALSE)
 # 'test/y_test.txt': Test labels
test_labels       = read.table(file.path(raiz, "y_test.txt"), header = FALSE)
#
# data wrangling
#
test <- cbind(test_labels, test_subject, test_set)
names(test) <- c("activity_id", "subject", names(test)[-c(1,2)] )
# names(test)

# Training Section --------------------------------------------------------
#
# Reading files, test data
#
raiz              <- file.path("UCI HAR Dataset", "train")

# 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample
train_subject      = read.table(file.path(raiz, "subject_train.txt"), header = FALSE)
# 'train/X_train.txt': train set
train_set          = read.table(file.path(raiz, "X_train.txt"), header = FALSE)
# 'train/y_train.txt': train labels
train_labels       = read.table(file.path(raiz, "y_train.txt"), header = FALSE)
#
# data wrangling
#
train <- cbind(train_labels, train_subject, train_set)
names(train) <- c("activity_id", "subject", names(train)[-c(1,2)] )
# names(train)

# Question 1 --------------------------------------------------------------
#
# Merges the training and the test sets to create one data set
#
datamerged <- tibble::as_tibble(rbind(test, train))

datamerged

# Question 2 --------------------------------------------------------------
#
# Extracts only the measurements on the mean and standard deviation for each measurement
#
# grep("mean", features[,2])
# grep("std", features[,2])

columns       <- grep("mean|std", features[,2]) + 2
newdatamerged <- datamerged[,c(1, 2, columns)]

newdatamerged

# Question 3 --------------------------------------------------------------
# 
# Uses descriptive activity names to name the activities in the data set
# 

library(tidyverse)

names(activity_labels) <- c("activity_id","activity")
newdatamerged <- newdatamerged %>% 
  left_join(activity_labels, by = "activity_id") %>%
  select(activity, everything(), -activity_id)

newdatamerged

# Question 4 --------------------------------------------------------------
#
# Appropriately labels the data set with descriptive variable names.
#
rows           <- grep("mean|std", features[,2])
variablesnames <- as.character(features[rows,2])
variablesnames <- gsub('\\(\\)',"", variablesnames)
variablesnames <- gsub('-',"", variablesnames)
variablesnames <- gsub('X',"Xaxis", variablesnames)
variablesnames <- gsub('Y',"Yaxis", variablesnames)
variablesnames <- gsub('Z',"Zaxis", variablesnames)

names(newdatamerged) <- c(names(newdatamerged)[1:2], variablesnames)

newdatamerged

# Question 5 --------------------------------------------------------------
#
# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#

library(tidyverse)

newdatamerged <- newdatamerged %>% gather(names(newdatamerged)[-c(1,2)], key = "variables", value = "value")

newdatamerged <- newdatamerged %>%
  group_by(activity, subject, variables) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  select(variables, activity, subject, average) %>%
  arrange(variables, activity, subject)

newdatamerged

# Please upload the tidy data set created in step 5 of the instructions. 
# Please upload your data set as a txt file created with write.table() using row.name=FALSE
# (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).

write.table(newdatamerged, "dataQuestion5.txt", row.name=FALSE)
