library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)


# Uncomment this code only if you want to create a fresh dataframe
# Otherwise the code loads an already created csv file for train errors

'
labeled.data <- readRDS("data-for-LOPART.rds")
pid_chrs <- unique(labeled.data$signals$pid.chr)
to_loop <- head(pid_chrs, 300)

penalties <- c(0.001, 0.01, 0.1, 1, 1.5, 2, 2.5, 3, 5, 10)
Train_Errors <- data.frame("profile" <- c(), "min_train_error" <- c(), "max_train_error" <- c())
result <- data.frame("penalty" <- c(), "errors_lopart" <- c(), "errors_opart" <- c(),
                     "train_errors_opart" <- c())

for(prof in pid_chrs){
  min_error <- Inf
  max_error <- 0
  signal_file <- paste("exp_data/", prof, "_signal.csv", sep="")
  labels_file <- paste("exp_data/", prof, "_labels.csv", sep="")
  data <- as.data.table(read.csv(file=signal_file))
  regions <- as.data.table(read.csv(file=labels_file))
  signal <- data$logratio
  names(signal) <- c("logratio")
  size <- length(signal)

  labels <- data.frame("starts" <- c(), "ends" <- c(), "breaks" <- c())

  for(i in 1:nrow(regions)){
    change <- regions[[i,3]]
    begin <- regions[[i,4]]
    end <- regions[[i,5]]
    if(change == "0breakpoints"){
      change <- 0
    }
    else{
      change <- 1
    }
    labels <- rbind(labels, data.frame(starts=begin,ends=end, breaks=change))
  }

  train_labels_count <- 0
  label_count <- nrow(labels)
  rem <- label_count %% 2

  if(rem == 0){
    train_labels_count <- (label_count/2)
  }
  if(rem == 1){
    train_labels_count <- round((label_count/2))
  }

  train_labels <- head(labels, train_labels_count)


  for(p in penalties){
    labelled_fit <- LabelledOpart::labelled_opart_gaussian(data$logratio, train_labels, p)
    positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
    opart_fit <- opart::opart_gaussian(data$logratio, p)
    positions_opart <- data.frame("position_op" <- c(opart_fit$end.vec[1:length(opart_fit$end.vec)-1]))

    errors_lopart <- 0
    errors_opart <- 0
    train_errors_opart <- 0

    set_lopart <- rep(0, label_count)
    set_opart <- rep(0, label_count)

    for(i in 1:nrow(labels)){
      lower <- labels[i,1]
      upper <- labels[i,2]
      changes <- labels[i,3]
      for(j in 1:nrow(positions)){
        val <- positions[j, 1]
        if(is.na(val) || val >= upper){
          break
        }
        if(val >= lower && val < upper){
          set_lopart[i] <- set_lopart[i] + 1
        }
      }
      for(k in 1:nrow(positions_opart)){
        val <- positions_opart[k, 1]
        if(is.na(val) || val >= upper){
          break
        }
        if(val >= lower && val < upper){
          set_opart[i] <- set_opart[i] + 1
        }
      }
    }
    temp <- 0
    if(rem == 0){
      temp <- (label_count/2) + 1
    }
    else{
      temp <- round((label_count)/2)
    }
    for(i in 1:(temp-1)){
      change <- labels[i,3]
      if(change == 1){
        train_errors_opart <- train_errors_opart + abs(set_opart[i] - 1)
      }
      else{
        train_errors_opart <- train_errors_opart + set_opart[i]
      }
    }
    for(i in temp:nrow(labels)){
      change <- labels[i,3]
      if(change == 1){
        errors_lopart <- errors_lopart + abs(set_lopart[i] - 1)
        errors_opart <- errors_opart + abs(set_opart[i] - 1)
      }
      else{
        errors_lopart <- errors_lopart + set_lopart[i]
        errors_opart <- errors_opart + set_opart[i]
      }
    }
    if(train_errors_opart < min_error){
      min_error = train_errors_opart
    }
    if(train_errors_opart > max_error){
      max_error = train_errors_opart
    }
    r1 <- data.frame(p, errors_lopart, errors_opart, train_errors_opart)
    names(r1) <- c("penalty", "errors_lopart", "errors_opart", "train_errors_opart")
    result <- rbind(result, r1)
  }
  r <- data.frame(paste("profile",prof,sep=""), min_error, max_error)
  names(r) <- c("profile", "min_train_error", "max_train_error")
  Train_Errors <- rbind(Train_Errors, r)
}
'

test_errors <- as.data.frame(read.csv("vignettes/TestErrors.csv"))
train_errors_opart <- as.data.frame(read.csv("vignettes/TrainErrors.csv"))
head(train_errors_opart)

test_errors_lopart <- data.frame("penalty" <- c(test_errors$penalty), "errors" <- c(test_errors$errors_lopart))
names(test_errors_lopart) <- c("penalty", "errors")
test_errors_opart <- data.frame("penalty" <- c(test_errors$penalty),"errors" <- c(test_errors$errors_opart))
names(test_errors_opart) <- c("penalty", "errors")

test_errors_lopart <- data.table(test_errors_lopart)
test_errors_opart <- data.table(test_errors_opart)

mean_errors_lopart <- test_errors_lopart[, mean(errors), by=penalty]
mean_errors_opart <- test_errors_opart[, mean(errors), by=penalty]

mean_errors_lopart$type <- paste("labelled", "opart")
mean_errors_opart$type <- "opart"

combined <- rbind(mean_errors_lopart, mean_errors_opart)
names(combined) <- c("Penalty", "AverageErrors", "type")

ggplot() + geom_point(data=combined, aes(x=Penalty, y=AverageErrors, col=type))
