library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)

add.x.var <- function(df, method){
  data.frame(df, method=factor(method, c("lopart", "opart")))
}

labeled.data <- readRDS("data-for-LOPART.rds")
pid_chrs <- unique(labeled.data$signals$pid.chr)
to_loop <- head(pid_chrs, 10)

result <- data.frame("profile" <- c(), "penalty" <- c(), "errors_lopart" <- c(), "errors_opart" <- c(),
                     "train_errors_opart" <- c())

for(prof in to_loop){
  signal_file <- paste("exp_data/", prof, "_signal.csv", sep="")
  labels_file <- paste("exp_data/", prof, "_labels.csv", sep="")
  data <- as.data.table(read.csv(file=signal_file))
  regions <- as.data.table(read.csv(file=labels_file))

  signal <- as.data.frame(data$logratio)
  names(signal) <- c("logratio")

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

  penalties <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  train_labels_count <- 0
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
    opart_fit <- opart::opart_gaussian(data$logratio, p)
    positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
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

    r <- data.frame(paste("profile",prof), p, errors_lopart, errors_opart, train_errors_opart)
    names(r) <- c("profile", "penalty", "errors_lopart", "errors_opart", "train_errors_opart")
    result <- rbind(result, r)
  }
}

result


comb_result1 <- data.frame(cbind(as.character(result$profile), result$penalty, result$errors_lopart))
names(comb_result1) <- c("profile", "penalty", "errors")

comb_result2 <- data.frame(cbind(as.character(result$profile), result$penalty, result$errors_opart))
names(comb_result2) <- c("profile", "penalty", "errors")


ggplot() + geom_point(aes(x=penalty,y=errors,col=method),
                      data=add.x.var(comb_result1, "lopart"))+
          geom_point(aes(x=penalty,y=errors,col=method),
                     data=add.x.var(comb_result2, "opart"))+
  facet_grid(method ~ profile, scales = "free")

