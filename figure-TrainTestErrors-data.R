library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)
if(!file.exists("data-for-LOPART.rds")){
  download.file(
    "http://jan.ucc.nau.edu/~th798/data/data-for-LOPART.rds",
    "data-for-LOPART.rds")
}

labeled.data <- readRDS("data-for-LOPART.rds")

pid.chr.i <- 80# error for 0 pen
test.fold <- 2
penalty <- 0
model.name <- "LOPART"

pid.chr.i <- 288
test.fold <- 1
penalty <- 1e5
model.name <- "LOPART"

pid.chr.i <- 355
test.fold <- 1

pid.chr.i <- 325
test.fold <- 1


signal.sizes <- labeled.data$signals[, .(N=.N), by=pid.chr][order(N)]
pid.chr.unique <- signal.sizes$pid.chr
for(pid.chr.i in seq_along(pid.chr.unique)){
  pid.chr <- pid.chr.unique[[pid.chr.i]]
  cache.csv <- file.path("LOPART", paste0(pid.chr, ".csv"))
  pid.chr.err <- if(file.exists(cache.csv)){
    data.table::fread(cache.csv)
  }else{
    cat(sprintf("%4d / %4d %s\n", pid.chr.i, length(pid.chr.unique), cache.csv))
    select.dt <- data.table(pid.chr)
    data.list <- list()
    for(data.type in names(labeled.data)){
      data.list[[data.type]] <- labeled.data[[data.type]][select.dt, on="pid.chr"]
    }
    if(any(is.na(data.list$regions$first.i))){
      NULL
    }else{
      set.seed(1)
      n.folds <- 2
      unique.folds <- 1:n.folds
      data.list$regions[, fold := sample(rep(unique.folds, l=.N))]
      computed.err <- data.table(test.fold=unique.folds)[, {
        fold.regions <- data.table(data.list$regions)
        fold.regions[, set := ifelse(fold==test.fold, "test", "train")]
        train.label.dt <- fold.regions[set=="train", .(
          start=first.i,
          end=last.i,
          breaks=ifelse(annotation=="1breakpoint", 1, 0))]
        model.list <- list(
          LOPART=train.label.dt,
          OPART=train.label.dt[0])
        pen.vec <- 10^seq(-5, 5, by=0.5)
        model.dt <- data.table(expand.grid(
          model.name=names(model.list),
          penalty=pen.vec))
        model.dt[, {
          model.labels <- model.list[[model.name]]
          ## fit <- LabelledOpart::labelled_opart_gaussian(
          ##   data.list$signals$logratio, model.labels, penalty)
          ## end.dt <- data.table(end=fit$end.vec)
          model.labels[, changes := breaks]
          fit <- LOPART::LOPART(
            data.list$signals$logratio, model.labels, penalty)
          end.dt <- fit$segments[, .(end)]
          change.dt <- end.dt[-.N, .(change=end+0.5)]
          meta.dt <- data.table(problem=pid.chr, model.name, penalty)
          fold.regions[, {
            err.list <- penaltyLearning::labelError(
              meta.dt,
              data.table(meta.dt, .SD),
              data.table(meta.dt, change.dt),
              change.var = "change",
              label.vars = c("first.i", "last.i"),
              problem.vars = "problem",
              model.vars = c("model.name", "penalty"))
            err.list$model.errors
          }, by=set]
        }, by=.(model.name, penalty)]
      }, by=test.fold]
      computed.err[set=="test", .(min=min(errors)), by=.(test.fold, model.name)]
      dir.create(dirname(cache.csv), showWarnings = FALSE, recursive = TRUE)
      data.table::fwrite(computed.err, cache.csv)
    }
  }#if cached else
}
