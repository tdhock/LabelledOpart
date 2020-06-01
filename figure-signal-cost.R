library(ggplot2)
library(data.table)
library(tikzDevice)
library(penaltyLearning)
library(latex2exp)
library(opart)
library(microbenchmark)
library(directlabels)

options(
  tikzDocumentDeclaration="\\documentclass[12pt]{article}",
  tikzMetricsDictionary="tikzMetricsNIPS")

set.seed(2)
signal <- c(
  rnorm(25, mean = 10),
  rnorm(25, mean = 7),
  rnorm(25, mean = 8),
  rnorm(25, mean = 5))
#outliers
signal[86] <- 10
labels <- data.table(
  "start" = c(20, 45, 80),
  "end" = c(30, 55, 90),
  "breaks" = c(1, 1, 0))
labels.dt <- data.table(
  labels,
  y=c(9, 4, 11))
signal.dt <- data.table(
  signal,
  position=seq_along(signal))
label.colors <- c(
  "1"="#ff7d7d",
  "0"="#f6c48f")
gg <- ggplot()+
  geom_rect(aes(
    xmin=start, xmax=end,
    fill=paste(breaks),
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    data=labels)+
  geom_point(aes(
    position, signal),
    data=signal.dt)+
  scale_fill_manual("label", values=label.colors)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))
label.list <- list(
  OPART=labels[0,],
  LOPART=labels)
seg.dt.list <- list()
change.dt.list <- list()
cost.dt.list <- list()
for(model.name in names(label.list)){
  label.dt <- data.table(label.list[[model.name]])
  fit <- LabelledOpart::labelled_opart_gaussian(signal, label.dt, 10)
  end <- as.integer(fit$end.vec)
  start <- as.integer(c(1, end[-length(end)]+1))
  Algorithm <- factor(model.name, names(label.list))
  model.segs <- signal.dt[
    data.table(start, end),
    data.table(mean=mean(signal), N=.N, start, end),
    by=.EACHI,
    nomatch=0L,
    on=list(position >= start, position <= end)]
  tau.dt <- with(fit, data.table(
    cand_cost,
    tau=seq_along(cand_cost)-1L,
    change=seq_along(cand_cost)-0.5
  ))[cand_cost != -Inf]
  cost.dt.list[[model.name]] <- data.table(Algorithm, tau.dt)
  stopifnot(sum(model.segs$N) == length(signal))
  seg.dt.list[[model.name]] <- data.table(Algorithm, model.segs)
  change.dt.list[[model.name]] <- model.segs[-1, data.table(
    Algorithm, change=start-0.5)]
}
seg.dt <- do.call(rbind, seg.dt.list)[, .(Algorithm, mean, start, end)]
change.dt <- do.call(rbind, change.dt.list)
cost.dt <- do.call(rbind, cost.dt.list)
abbrev.vec <- c(
  data="data and models",
  cost="cost of last change")
yfac <- function(l){
  factor(abbrev.vec[[l]], abbrev.vec)
}
COST <- function(dt){
  data.table(y.var=yfac("cost"), dt)
}
DATA <- function(dt){
  data.table(y.var=yfac("data"), dt)
}
sig.text.dt <- signal.dt[c(1, .N), .(
  signal, i=position, position=position+c(-1,1), hjust=c(1, 0))]
cost.text.dt <- cost.dt[
  Algorithm=="OPART"
][c(1, .N), .(
  pos=change, cand_cost, tau,
  hjust=c(0.5, 0.5),
  vjust=c(2.5, -1.5)
)]
sig.color <- "grey50"
min.dt <- cost.dt[, .SD[which.min(cand_cost)], by=Algorithm]
min.dt[, hjust := ifelse(Algorithm=="OPART", 0, 1)]
min.dt[, y := cost.dt[is.finite(cand_cost), mean(range(cand_cost))] ]
gg <- ggplot()+
  geom_text(aes(
    position, signal, hjust=hjust,
    label=sprintf("$x_{%d}$", i)),
    color=sig.color,
    data=DATA(sig.text.dt))+
  geom_rect(aes(
    xmin=start, xmax=end,
    fill=paste(breaks),
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    data=labels)+
  scale_fill_manual("label", values=label.colors)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(y.var ~ ., scales="free")+
  geom_vline(aes(
    xintercept=change,
    size=Algorithm,
    color=Algorithm),
    data=change.dt)+
  geom_segment(aes(
    start-0.5, mean,
    size=Algorithm,
    color=Algorithm,
    xend=end+0.5, yend=mean),
    data=DATA(seg.dt))+
  geom_point(aes(
    position, signal),
    color=sig.color,
    shape=1,
    data=DATA(signal.dt))+
  scale_size_manual(values=c(
    OPART=1,
    LOPART=0.5))+
  scale_color_manual(values=c(
    OPART="deepskyblue",
    LOPART="black"))+
  ylab("")+
  scale_x_continuous(
    "position",
    breaks=seq(0, 100, by=10))+
  geom_text(aes(
    start, y,
    label=sprintf("$\\underline p_{%d}=%d$", seq_along(start), start)),
    hjust=1,
    data=DATA(labels.dt))+
  geom_text(aes(
    (start+end)/2, y+1,
    label=sprintf("$y_{%d}=%d$", seq_along(start), breaks)),
    data=DATA(labels.dt))+
  geom_text(aes(
    end, y,
    label=sprintf("$\\overline p_{%d}=%d$", seq_along(start), end)),
    hjust=0,
    data=DATA(labels.dt))+
  geom_text(aes(
    pos, cand_cost,
    hjust=hjust,
    vjust=vjust,
    label=sprintf("$\\tau = %d$", tau)),
    data=COST(cost.text.dt))+
  geom_text(aes(
    change, y,
    hjust=hjust,
    color=Algorithm,
    label=sprintf("$\\tau^*_{%d} = %d$", nrow(signal.dt), tau)),
    data=COST(min.dt))+
  geom_point(aes(
    change, cand_cost,
    color=Algorithm, shape=Algorithm),
    data=COST(cost.dt))+
  scale_shape_manual(values=c(OPART=1, LOPART=2))+
  theme(legend.position="bottom")
w=6
h=3
tikz("figure-signal-cost-standAlone.tex", width=w, height=h, standAlone=TRUE)
print(gg)
dev.off()
system("pdflatex figure-signal-cost-standAlone")
tikz("figure-signal-cost.tex", width=w, height=h)
print(gg)
dev.off()

