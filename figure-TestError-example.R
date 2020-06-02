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

pid.chr <- "20133.X"
test.fold <- 1
select.dt <- data.table(pid.chr)
data.list <- list()
for(data.type in names(labeled.data)){
  data.list[[data.type]] <- labeled.data[[data.type]][select.dt, on="pid.chr"]
}
set.seed(1)
n.folds <- 2
unique.folds <- 1:n.folds
data.list$regions[, fold := sample(rep(unique.folds, l=.N))]
computed.err <- data.table(test.fold=1)[, {
  fold.regions <- data.table(data.list$regions)
  fold.regions[, set := ifelse(fold==test.fold, "test", "train")]
  print(fold.regions)
  train.label.dt <- fold.regions[set=="train", .(
    start=first.i,
    end=last.i,
    breaks=ifelse(annotation=="1breakpoint", 1, 0))]
  model.list <- list(
    LOPART=train.label.dt,
    OPART=train.label.dt[0])
  pen.vec <- 10^seq(-1, 1, by=0.1)
  pen.vec <- 4
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
    print(model.name)
    print(fit)
    end.dt <- fit$segments[, .(end)]
    change.dt <- end.dt[-.N, .(change=end+0.5)]
    meta.dt <- data.table(problem=pid.chr, Algorithm=model.name, pen.num=penalty)
    fold.regions[, {
      err.list <- penaltyLearning::labelError(
        meta.dt,
        data.table(meta.dt, .SD),
        data.table(meta.dt, change.dt),
        change.var = "change",
        label.vars = c("first.i", "last.i"),
        problem.vars = "problem",
        model.vars = c("Algorithm", "pen.num"))
      err.list$label.errors
    }, by=set]
  }, by=.(model.name, penalty)]
}, by=test.fold]

stats.err <- computed.err[, .(
  errors=sum(fp+fn)
), by=.(test.fold, model.name, set, penalty)]
stats.err[set=="test", .(
  min=min(errors),
  penalties=.N
), by=.(test.fold, model.name)]

ggplot()+
  geom_line(aes(
    penalty, errors, color=model.name),
    data = stats.err)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(set + test.fold ~ .)+
  scale_x_log10()

ggplot()+
  geom_rect(aes(
    xmin=first.i, xmax=last.i,
    ymin=-Inf, ymax=Inf,
    fill=annotation),
    color="grey",
    data=data.list$regions)+
  geom_point(aes(
    seq_along(logratio), logratio),
    data = data.list$signals)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  xlim(50, 100)

