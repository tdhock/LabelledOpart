library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)

data <- as.data.table(read.csv(file="exp_data/8.11_signal.csv"))
regions <- as.data.table(read.csv(file="exp_data/8.11_labels.csv"))


signal <- as.data.frame(data$logratio)


#regions <- head(regions, 5)
#signal <- head(signal,5100)
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

#specify number of labels for train set to labelled opart
#train_labels <- head(labels, 1)
train_labels <- labels

label_count <- nrow(labels)

labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal$logratio, train_labels, 0.05)
fpop_fit <- fpop::Fpop(signal$logratio, 0.05)
pdpa_fit <- Segmentor3IsBack::Segmentor(data=signal$logratio, model=2,
                                        Kmax=length(labelled_fit$end.vec))
pdpa_breaks <-  as.numeric(pdpa_fit@breaks[length(labelled_fit$end.vec),])


positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
colnames(positions) <- c("position")

positions_fpop <- data.frame("position_fpop" <- c(fpop_fit$t.est[1:length(fpop_fit$t.est)-1]))
colnames(positions_fpop) <- c("position_fpop")


positions_pdpa <- data.frame("position_pdpa" <- c(pdpa_breaks[1:length(pdpa_breaks)-1]))
colnames(positions_pdpa) <- c("position_pdpa")


dataset <- signal$logratio

segments_lp <- data.frame("start" <- c(), "end" <- c(), "val" <- c())
segments_fpop <- data.frame("start" <- c(), "end" <- c(), "val" <- c())
segments_pdpa <- data.frame("start" <- c(), "end" <- c(), "val" <- c())

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
if(nrow(positions_fpop) > 0){
  for(i in 1:nrow(positions_fpop)){
    end = positions_fpop[i,]
    segments_fpop <- rbind(segments_fpop, data.frame(start=lp_prev, end=end,val=mean(dataset[lp_prev:end])))
    lp_prev = positions_fpop[i,]
  }
}
segments_fpop <- rbind(segments_fpop, data.frame(start=lp_prev, end=last, val=mean(dataset[lp_prev:last])))

if(nrow(positions_fpop) == 0){
  positions_fpop <- data.frame("position_fpop" <- c(length(dataset)))
  colnames(positions_fpop) <- c("position_fpop")
}

lp_prev = 1
end = 1
last = length(dataset)
if(nrow(positions_pdpa) > 0){
  for(i in 1:nrow(positions_pdpa)){
    end = positions_pdpa[i,]
    segments_pdpa <- rbind(segments_pdpa, data.frame(start=lp_prev, end=end,val=mean(dataset[lp_prev:end])))
    lp_prev = positions_pdpa[i,]
  }
}
segments_pdpa <- rbind(segments_pdpa, data.frame(start=lp_prev, end=last, val=mean(dataset[lp_prev:last])))

if(nrow(positions_pdpa) == 0){
  positions_pdpa <- data.frame("position_pdpa" <- c(length(dataset)))
  colnames(positions_pdpa) <- c("position_pdpa")
}


add.x.var <- function(df, x.var){
  data.frame(df, x.var=factor(x.var, c("lopart", "fpop", "pdpa")))
}

ggplot() + geom_point(aes(position, signal), data = add.x.var(sel_data, "lopart"), shape = 1)+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(one_labels, "lopart"),
                                 color="grey",
                                 fill="pink")+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(zero_labels, "lopart"),
                                 color="grey",
                                 fill="lemonchiffon2")+

  geom_vline(aes(xintercept=position),
             data=add.x.var(positions, "lopart"),
             color="green",
             size=1.2,
             linetype="dashed")+
  geom_segment(aes(x=start, xend=end, y=val, yend=val), col=I("green"),
               data=add.x.var(segments_lp, "lopart"), size = 1.2)+

  geom_point(aes(position, signal), data = add.x.var(sel_data, "fpop"), shape = 1) +
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(one_labels, "fpop"),
                                 color="grey",
                                 fill="pink")+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(zero_labels, "fpop"),
                                 color="grey",
                                 fill="lemonchiffon2")+
  geom_vline(aes(xintercept=position_fpop),
             data=add.x.var(positions_fpop, "fpop"),
             color="green",
             size=1.2,
             linetype="dashed")+
  geom_segment(aes(x=start, xend=end, y=val, yend=val), col=I("green"),
               data=add.x.var(segments_fpop,"fpop"), size = 1.2)+

  geom_point(aes(position, signal), data = add.x.var(sel_data, "pdpa"), shape = 1) +
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(one_labels, "pdpa"),
                                 color="grey",
                                 fill="pink")+
  penaltyLearning::geom_tallrect(aes(xmin=starts,xmax=ends),
                                 data=add.x.var(zero_labels, "pdpa"),
                                 color="grey",
                                 fill="lemonchiffon2")+
  geom_vline(aes(xintercept=position_pdpa),
             data=add.x.var(positions_pdpa, "pdpa"),
             color="green",
             size=1.2,
             linetype="dashed")+
  geom_segment(aes(x=start, xend=end, y=val, yend=val), col=I("green"),
               data=add.x.var(segments_pdpa,"pdpa"), size = 1.2)+
  annotate(geom="text",x=15,y=0,label="0 breakpoints",color="red")+
  annotate(geom="text",x=68,y=0,label="1 breakpoint",color="red")+
  annotate(geom="text",x=110,y=0,label="0 breakpoints",color="red")+
  #annotate(geom="text",x=15,y=-0.2,label="train label",color="red")+
  #annotate(geom="text",x=68,y=-0.2,label="train label",color="red")+
  #annotate(geom="text",x=110,y=-0.2,label="test label",color="red")+

  facet_grid(x.var ~ ., scales="free")

