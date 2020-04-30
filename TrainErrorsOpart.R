library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)

labeled.data <- readRDS("data-for-LOPART.rds")
pid_chrs <- unique(labeled.data$signals$pid.chr)

penalties <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Train_Errors <- data.frame("profile" <- c(), "min_train_error" <- c(), "max_train_error" <- c())

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
  label_count <- nrow(labels)
  rem <- label_count %% 2
  for(p in penalties){
    opart_fit <- opart::opart_gaussian(data$logratio, p)
    positions_opart <- data.frame("position_op" <- c(opart_fit$end.vec[1:length(opart_fit$end.vec)-1]))

    errors_opart <- 0
    train_errors_opart <- 0

    set_opart <- rep(0, label_count)

    for(i in 1:nrow(labels)){
      lower <- labels[i,1]
      upper <- labels[i,2]
      changes <- labels[i,3]
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
    if(train_errors_opart < min_error){
      min_error = train_errors_opart
    }
    if(train_errors_opart > max_error){
      max_error = train_errors_opart
    }
  }
  r <- data.frame(paste("profile",prof,sep=""), min_error, max_error)
  names(r) <- c("profile", "min_train_error", "max_train_error")
  Train_Errors <- rbind(Train_Errors, r)
}
Train_Errors
