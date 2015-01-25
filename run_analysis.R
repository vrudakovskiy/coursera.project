BaseDir <- "UCI HAR Dataset"

dataset.file <- function(path) {
  paste(c(".", BaseDir, path), collapse = "/")
}

dataset.read <- function(name) {
  subject <- read.table(dataset.file(c(name, paste("subject_", name, ".txt", sep = ""))), sep = " ")
  y <- read.table(dataset.file(c(name, paste("y_", name, ".txt", sep = ""))), sep = " ")
  X <- read.table(dataset.file(c(name, paste("X_", name, ".txt", sep = ""))), strip.white = TRUE)
  list("subject" = subject, "activity" = y, "signal" = X) 
}

activities <- read.table(dataset.file("activity_labels.txt"), sep = " ")
colnames(activities) <- c("ID", "Name")

features <- read.table(dataset.file("features.txt"), sep = " ")
colnames(features) <- c("ID", "Name")


test <- dataset.read("test")
train <- dataset.read("train")
full <- list(
  "subject" = rbind(test$subject, train$subject),
  "activity" = rbind(test$activity, train$activity),
  "signal" = rbind(test$signal, train$signal)
)

mean_columns <- grepl("mean\\(\\)", features$Name)
std_columns <- grepl("std\\(\\)", features$Name)

result <- full$signal[, which(mean_columns | std_columns)]
colnames(result) <- features$Name[which(mean_columns | std_columns)]
result$Subject <- full$subject[,1]
result$Activity <- activities[match(full$activity[,1], activities$ID), "Name"]

library(reshape2)
result <- melt(result, id.vars = c("Subject", "Activity"))
result <- dcast(result, Subject + Activity ~ variable, mean, na.rm = TRUE)

write.table(result, "tidy.txt", row.names = FALSE)
