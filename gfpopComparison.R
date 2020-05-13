library(data.table)
data(profile614chr2, package="gfpop")
profile614chr2$probes[, change.after := floor(
  c(diff(position)/2+position[-.N], NA)) ]
sngraph <- function(n.segs, type, gap){
  stopifnot(is.integer(n.segs), length(n.segs)==1, n.segs >= 1)
  s <- n.segs-1
  seg.vec <- 1:s
  gfpop::graph(
    gfpop::StartEnd(start=0, end=s),
    gfpop::Edge(seg.vec-1, seg.vec, type, gap=gap), all.null.edges = TRUE)
}
select.dt <- rbind(
  data.table(n.segments = c(3, 7, 13), graph.name = "std", gap = 0))
#data.table(n.segments = 13, graph.name = "abs", gap = 1))
seg.dt.list <- list()
for(model.i in 1:nrow(select.dt)){
  model.info <- select.dt[model.i]
  cat(sprintf("%4d / %4d models\n", model.i, nrow(select.dt)))
  g <- model.info[, sngraph(as.integer(n.segments), graph.name, gap=gap)]
  fit <- gfpop::gfpop(
    profile614chr2$probes$logratio,
    mygraph = g, type = "mean")
  end.i <- fit$changepoints
  change.i <- end.i[-length(end.i)]
  change.pos <- profile614chr2$probes$change.after[change.i]
  seg.dt.list[[model.i]] <- data.table(
    model.info,
    segStart=c(profile614chr2$probes[1, position], change.pos),
    segEnd=c(change.pos, profile614chr2$probes[.N, position]),
    mean=fit$parameters)
}
seg.dt <- do.call(rbind, seg.dt.list)

selData <- as.data.table(profile614chr2$probes)
labels_lp <- as.data.table(profile614chr2$labels)
modifiedLabels <- data.frame("start" = c(), "end" = c(), "breaks" = c())
for(i in 1:nrow(labels_lp)){
  min <- labels_lp[[i,1]]
  max <- labels_lp[[i,2]]
  annotation <- labels_lp[[i,3]]
  positions <- selData[position >= min & position <= max][,1]
  change <- 0
  if(annotation == "1breakpoint"){
    change <- 1
  }
  else{
    change <- 0
  }
  modifiedLabels <- rbind(modifiedLabels, data.frame(start=min(positions),                                                      end=max(positions), breaks=change))
}

labels_lp <- data.frame("start" = c(), "end" = c(), "breaks" = c())
for(i in 1:nrow(modifiedLabels)){
  start <- modifiedLabels[i, 1]
  end <- modifiedLabels[i, 2]
  change <- modifiedLabels[i, 3]
  index1 <- which(selData[,position] == start)
  index2 <- which(selData[,position] == end)
  labels_lp <- rbind(labels_lp, data.frame(start = index1, end = index2, breaks = change))
}
labels_lp <- labels_lp[order(labels_lp$start),]

labelled_fit <- LabelledOpart::labelled_opart_gaussian(selData$logratio, labels_lp, 15)

positions <- selData[labelled_fit$end.vec]
segments <- data.frame("n.segments" <- c(), "graph.name"<- c(), "gap"<- c(), "segStart" = c(), "segEnd" = c(), "mean" = c())
prev <- 1
for(pos in labelled_fit$end.vec){
  avg <- mean(selData[prev:pos,]$logratio)
  segments <- rbind(segments, data.frame(n.segments=13,graph.name="lopart",gap=1,segStart=selData[prev]$position, segEnd=selData[pos]$position, mean=avg))
  prev <- pos
}

seg.dt <- rbind(seg.dt, segments)
select.dt <- rbind(
  data.table(n.segments = c(3, 7, 13), graph.name = "std", gap = 0),
  data.table(n.segments = 13, graph.name = "lopart", gap = 1))

some.segs <- seg.dt[select.dt, on=list(n.segments, graph.name), allow.cartesian=TRUE]
some.change <- some.segs[min(segStart) < segStart]
some.change[, change := segStart]

err.dt <- some.segs[, {
  change.dt <- .SD[min(segStart) < segStart]
  change.dt[, change := segStart]
  change.dt[, prob := 1]
  change.dt[, nseg := n.segments]
  model.dt <- data.table(nseg=n.segments, prob=1)
  penaltyLearning::labelError(
    model.dt,
    data.table(prob=1, profile614chr2$labels),
    change.dt,
    change.var="change",
    model.vars="nseg",
    label.vars=c("labelStart", "labelEnd"),
    problem.vars="prob")$label.errors
}, by=list(graph.name, n.segments)]
err.dt[, list(
  fp=sum(fp),
  fn=sum(fn),
  errors=sum(fp+fn)
), by=list(graph.name, n.segments)]
win <- function(min,max)data.table(windowStart=min*1e5,windowEnd=max*1e5)
windows <- rbind(
  win(  65,  71),
  win( 148, 171),
  win( 354, 361),
  win(1059,1065))
mb.fac <- 1e6
wfac <- function(x){
  factor(x, c("6.5-7.1", "14.8-17.1", "35.4-36.1", "105.9-106.5"))
}
windows[, window := wfac(sprintf(
  "%.1f-%.1f", windowStart/mb.fac, windowEnd/mb.fac))]
setkey(windows, windowStart, windowEnd)
f <- function(dt, key.vec){
  setkeyv(dt, key.vec)
  dt
}
profile614chr2$probes[, pos0 := position]
over.list <- list(
  changes=f(some.change, c("change", "segStart")),
  segments=f(some.segs, c("segStart", "segEnd")),
  labels=f(profile614chr2$labels, c("labelStart", "labelEnd")),
  errors=f(err.dt, c("labelStart", "labelEnd")),
  probes=f(profile614chr2$probes, c("position", "pos0")))
join.list <- lapply(over.list, foverlaps, windows, nomatch=0L)


show.err <- join.list$errors[, list(
  fp=sum(fp),
  fn=sum(fn),
  errors=sum(fp+fn)
), by=list(graph.name, n.segments)]
library(ggplot2)
br <- c(6.5,7.0,seq(15,17,by=0.5),35.5,36,106,106.5)
names(br) <- as.character(br)
gg.out <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  coord_cartesian(expand=FALSE)+
  facet_grid(
    graph.name + n.segments ~ window,
    ##labeller=label_both,
    scales="free",space="free")+
  penaltyLearning::geom_tallrect(aes(
    xmin=labelStart/mb.fac, xmax=labelEnd/mb.fac, fill=annotation),
    color="grey",
    data=join.list$labels)+
  scale_fill_manual(
    "label",
    values=penaltyLearning::change.colors)+
  penaltyLearning::geom_tallrect(aes(
    xmin=labelStart/mb.fac, xmax=labelEnd/mb.fac, linetype=status),
    fill=NA,
    size=1,
    color="black",
    data=join.list$errors)+
  scale_linetype_manual(
    "error type",
    limits=c("correct",
             "false negative",
             "false positive"),
    values=c(correct=0,
             "false negative"=3,
             "false positive"=1))+
  geom_point(aes(
    position/mb.fac, logratio),
    shape=1,
    color="grey50",
    data=join.list$probes)+
  scale_y_continuous(
    "Logratio (Measure of DNA copy number)",
    breaks=seq(-2, 2, by=2)
  )+
  scale_x_continuous("Position on chr2 (mega bases)", breaks=br)+
  scale_color_manual(values=c(
    std="green",
    lopart="deepskyblue"))+
  geom_segment(aes(
    ifelse(segStart < windowStart, windowStart, segStart)/mb.fac, mean,
    color=graph.name,
    xend=ifelse(windowEnd < segEnd, windowEnd, segEnd)/mb.fac, yend=mean),
    data=join.list$segments,
    size=1)+
  geom_vline(aes(
    xintercept=change/mb.fac, color=graph.name),
    linetype="dashed",
    size=0.75,
    data=join.list$changes)+
  geom_text(aes(
    14.8, -3, label=sprintf(
      " %d label error%s for %d segment %s model",
      errors, ifelse(errors==1, "", "s"), n.segments, graph.name)),
    hjust=0,
    vjust=0,
    data=data.table(show.err, window=wfac("14.8-17.1")))
##png("figure-relevant-changes-copy-number.png", 8, 4, units="in", res=300, type="cairo")
print(gg.out)
##dev.off()
