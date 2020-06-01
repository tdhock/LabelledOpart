library(ggplot2)
library(data.table)
library(tikzDevice)
library(penaltyLearning)
library(latex2exp)
library(opart)
library(microbenchmark)
library(directlabels)

signal <- c(
  rnorm(25, mean = 10),
  rnorm(25, mean = 8),
  rnorm(25, mean = 8),
  rnorm(25, mean = 5))
#outliers
signal[86] <- 12
signal[10] <- 5
signal[26] <- 12
signal[29] <- 5
signal[51] <- 5

labels <- data.frame(
  "start" = c(24, 49, 84),
  "end" = c(29, 54, 89),
  "breaks" = c(1, 1, 0))

labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, 10)
candidates <- labelled_fit$cand_cost[2:length(labelled_fit$cand_cost)]
position <- c(1:94)
breakpoints <- labelled_fit$end.vec

positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
positions <- as.data.frame("position" <- c(positions[,1 ] + 0.5))


selData <- as.data.frame(cbind(position, candidates, signal))
selData <- selData[selData$candidates != -1,]

cand_data <- as.data.frame(cbind(position, candidates))
cand_data <- cand_data[cand_data$candidates != -1,]

sig_data <- as.data.frame(cbind(position, signal))

cost_plot <- ggplot() + geom_point(aes(position, candidates), data = cand_data, shape = 1)


regions <- data.frame("start" = c(49), "end" = c(54), "breaks" = c(1))

sig_plot <- ggplot() + geom_point(aes(position, signal), data = sig_data, shape = 1)+
  penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),                                                      color="pink",fill="pink",data=regions)


#making lopart work like opart to get candidate cost for opart
labels1 <- data.frame("start" = c(1), "end" = c(1), "breaks" = c(0))
labelled_fit1 <- LabelledOpart::labelled_opart_gaussian(signal, labels1, 10)
candidates1 <- labelled_fit1$cand_cost[2:length(labelled_fit1$cand_cost)]
candidates1 <- candidates1[candidates1 != -1]

add.x.var <- function(df, x.var){
  data.frame(df, x.var=factor(x.var, c("cost", "signal", "cost_opart")))
}

cand_data <- data.frame("position" <- c(1:94), "cost_val" <- c(candidates[candidates != -1]))
cand_data1 <- data.frame("position" <- c(1:94), "cost_val" <- c(candidates1[1:94]))
sig_data <- data.frame("position_sig" <- c(1:94), "value" <- c(signal[1:94]))
colnames(sig_data) <- c("position_sig", "value")

labels <- data.frame("start" = c(24, 49, 84), "end" = c(29, 54, 89), "breaks" = c(1, 1, 0))

comb_data1 <- data.frame("position" <- c(1:94) + 0.5, "cost_val" <- c(candidates[candidates!=-1]),
                         "model" <- rep("lopart", 94))
colnames(comb_data1) <- c("position", "cost_val", "model")

comb_data2 <- data.frame("position" <- c(1:94) + 0.5, "cost_val" <- c(candidates1[1:94]),
                         "model" <- rep("opart", 94))
colnames(comb_data2) <- c("position", "cost_val", "model")

comb_data <- rbind(comb_data1, comb_data2)

prev_break <- breakpoints[1]
one_labels <- data.frame("start" = c(24, 49), "end" = c(29, 54), "breaks" = c(1, 1))
zero_labels <- data.frame("start" = c(84), "end" = c(89), "breaks" = c(0))



for(i in c(55:88)){
  #mean1 <- mean(signal[1:prev_break])
  #mean2 <- mean(signal[prev_break:i])
  #mean3 <- mean(signal[i:94])

  segments_opart <- data.frame("start" <- c(), "end" <- c(), "mean" <- c(), "type" <- c())

  chpts <- labelled_fit1$end.vec[labelled_fit1$end.vec < i]
  prev = 1
  for(j in 1:length(chpts)){
    m <- mean(signal[prev:chpts[j]])
    segments_opart <- rbind(segments_opart, data.frame(start=prev+0.5,end=chpts[j]+0.5,mean=m,type="opart"))
    colnames(segments_opart) <- c("start", "end", "mean", "type")
    prev = chpts[j]
  }
  segments_opart <- rbind(segments_opart, data.frame(start=prev+0.5,end=i+0.5,mean=mean(signal[prev:i]),type="opart"))
  colnames(segments_opart) <- c("start", "end", "mean", "type")

  segments_opart <- rbind(segments_opart, data.frame(start=i+0.5,end=94,mean=mean(signal[i:94]),type="opart"))
  colnames(segments_opart) <- c("start", "end", "mean", "type")


  segments <- data.frame("start" <- c(), "end" <- c(), "mean" <- c(), "type" <- c())
  chpts <- labelled_fit$end.vec[labelled_fit$end.vec < i]
  prev = 1
  for(j in 1:length(chpts)){
    m <- mean(signal[prev:chpts[j]])
    segments <- rbind(segments, data.frame(start=prev+0.5,end=chpts[j]+0.5,mean=m,type="lopart"))
    colnames(segments) <- c("start", "end", "mean", "type")
    prev = chpts[j]
  }
  segments <- rbind(segments, data.frame(start=prev+0.5,end=i+0.5,mean=mean(signal[prev:i]),type="lopart"))
  colnames(segments) <- c("start", "end", "mean", "type")

  segments <- rbind(segments, data.frame(start=i+0.5,end=94,mean=mean(signal[i:94]),type="lopart"))
  colnames(segments) <- c("start", "end", "mean", "type")

  #segments <- data.frame("start" <- c(1,prev_break+0.5,i+0.5),
  #                       "end" <- c(prev_break+0.5,i+0.5,94),
  #                       "mean" <- c(mean1, mean2, mean3),
  #                       "type" <- c("lopart", "lopart", "lopart"))
  #colnames(segments) <- c("start", "end", "mean", "type")
  segments <- rbind(segments, segments_opart)
  colnames(segments) <- c("start", "end", "mean", "type")


  opart_cp <- as.data.frame("position" <- c(labelled_fit1$end.vec[labelled_fit1$end.vec < i]))
  colnames(opart_cp) <- c("position")
  opart_cp$type <- "opart"

  lopart_cp <- as.data.frame("position" <- c(labelled_fit$end.vec[labelled_fit$end.vec < i]))
  colnames(lopart_cp) <- c("position")
  lopart_cp$type <- "lopart"

  comb_positions <- rbind(lopart_cp, opart_cp)

  plot <- ggplot() + geom_point(aes(position, cost_val, color=model), data=add.x.var(comb_data, "cost")) +
    geom_point(aes(position_sig, value), data=add.x.var(sig_data, "signal"))+
    penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),color="pink",fill="pink",data=add.x.var(one_labels, "signal"))+
    penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),color="yellow",fill="yellow",data=add.x.var(zero_labels, "signal"))+
    geom_vline(aes(
      xintercept=(i + 0.5)),
      data=positions,
      color="green",
      size=0.8,
      linetype="dashed")+
    geom_vline(aes(
      xintercept=(prev_break + 0.5)),
      data=add.x.var(positions,"signal"),
      col="green",
      size=0.8,
      linetype="dashed")+
    geom_vline(aes(
      xintercept=(position + 0.5),col=type),
      data=add.x.var(comb_positions,"signal"),
      size=0.8,
      linetype="dashed")+
    geom_segment(aes(x=start, xend=end, y=mean, yend=mean, col=type),
                 size=0.8,
                 data=add.x.var(segments, "signal"))+
    geom_text(aes(x=86.5,y=12.5),label="0 label",color="red", data=add.x.var(positions, "signal"))+
    geom_text(aes(x=51.5,y=12.5),label="1 label",color="red", data=add.x.var(positions, "signal"))+
    geom_text(aes(x=26.5,y=12.5),label="1 label",color="red", data=add.x.var(positions, "signal"))+
    facet_grid(x.var ~ ., scales="free")
  print(plot)
}

