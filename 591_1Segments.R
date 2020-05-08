library(ggplot2)
library(data.table)
library(tikzDevice)
library(penaltyLearning)
library(latex2exp)
library(opart)
library(microbenchmark)
library(directlabels)

data <- as.data.table(read.csv(file="exp_data/591.1_signal.csv"))
regions <- as.data.table(read.csv(file="exp_data/591.1_labels.csv"))
signal <- as.data.frame(data$logratio)
names(signal) <- c("logratio")
length_signal <- nrow(signal)
sel_data <- as.data.frame(cbind(c(1:length_signal), signal$logratio))
names(sel_data) <- c("position", "signal")

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

one_labels <- labels[labels$breaks == 1,]
zero_labels <- labels[labels$breaks == 0,]

label_count <- nrow(labels)
train_labels_count <- 0
rem <- label_count %% 2

if(rem == 0){
  train_labels_count <- (label_count/2)
}
if(rem == 1){
  train_labels_count <- round((label_count/2))
}

train_labels <- head(labels, train_labels_count)

labelled_fit <- LabelledOpart::labelled_opart_gaussian(data$logratio, train_labels, 2)
opart_fit <- opart::opart_gaussian(data$logratio, 2)

positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
colnames(positions) <- c("position")

positions_opart <- data.frame("position_op" <- c(opart_fit$end.vec[1:length(opart_fit$end.vec)-1]))
colnames(positions_opart) <- c("position")

dataset <- data$logratio
segments_lp <- data.frame("start" <- c(), "end" <- c(), "val" <- c())
segments_op <- data.frame("start" <- c(), "end" <- c(), "val" <- c())

lp_prev = 1
end = 1
last = length(dataset)
for(i in 1:nrow(positions)){
  end = positions[i,]
  segments_lp <- rbind(segments_lp, data.frame(start=lp_prev, end=end, val=mean(dataset[lp_prev:end])))
  lp_prev = positions[i,]
}
segments_lp <- rbind(segments_lp, data.frame(start=lp_prev, end=last, val=mean(dataset[lp_prev:last])))

lp_prev = 1
end = 1
last = length(dataset)
for(i in 1:nrow(positions_opart)){
  end = positions_opart[i,]
  segments_op <- rbind(segments_op, data.frame(start=lp_prev, end=end, val=mean(dataset[lp_prev:end])))
  lp_prev = positions_opart[i,]
}
segments_op <- rbind(segments_op, data.frame(start=lp_prev, end=last, val=mean(dataset[lp_prev:last])))


add.x.var <- function(df, x.var){
  data.frame(df, x.var=factor(x.var, c("lopart", "opart")))
}

ggplot() + geom_point(aes(position, signal), data = add.x.var(sel_data, "lopart"), shape = 1)+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(one_labels, "lopart"),
                                 color="pink",
                                 fill="pink")+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(zero_labels, "lopart"),
                                 color="yellow",
                                 fill="yellow")+

  geom_vline(aes(xintercept=position),
             data=add.x.var(positions, "lopart"),
             color="green",
             size=1,
             linetype="dashed")+
  geom_segment(aes(x=start, xend=end, y=val, yend=val), col=I("green"),
               data=add.x.var(segments_lp, "lopart"), size = 1)+

  geom_point(aes(position, signal), data = add.x.var(sel_data, "opart"), shape = 1) +
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(one_labels, "opart"),
                                 color="pink",
                                 fill="pink")+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(zero_labels, "opart"),
                                 color="yellow",
                                 fill="yellow")+
  geom_vline(aes(xintercept=position_op),
             data=add.x.var(positions_opart, "opart"),
             color="green",
             size=1,
             linetype="dashed")+
  geom_segment(aes(x=start, xend=end, y=val, yend=val), col=I("green"),
               data=add.x.var(segments_op,"opart"), size = 1)+
  annotate(geom="text",x=1500,y=-1,label="0 breakpoints",color="red")+
  annotate(geom="text",x=4750,y=-1,label="1 breakpoint",color="red")+
  annotate(geom="text",x=1500,y=-1.5,label="train label",color="red")+
  annotate(geom="text",x=4750,y=-1.5,label="test label",color="red")+
  facet_grid(x.var ~ ., scales="free")
