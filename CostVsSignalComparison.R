library(ggplot2)
library(data.table)
library(tikzDevice)
library(penaltyLearning)
library(latex2exp)
library(opart)
library(microbenchmark)
library(directlabels)

signal <- c(rnorm(25, mean = 10), rnorm(25, mean = 5), rnorm(25, mean = 10), rnorm(25, mean = 5))

#outlier at 86
signal[86] <- 12

labels <- data.frame("start" = c(24, 49, 84), "end" = c(29, 54, 89), "breaks" = c(1, 1, 0))

labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, 10)
candidates <- labelled_fit$cand_cost[2:length(labelled_fit$cand_cost)]
position <- c(1:94)
breakpoints <- labelled_fit$end.vec

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
labelled_fit1 <- LabelledOpart::labelled_opart_gaussian(signal, labels1, 2)
candidates1 <- labelled_fit1$cand_cost[2:length(labelled_fit1$cand_cost)]
candidates1 <- candidates1[candidates1 != -1]

add.x.var <- function(df, x.var){
  data.frame(df, x.var=factor(x.var, c("cost", "signal", "cost_opart")))
}


cand_data <- data.frame("position" <- c(1:94), "cost_val" <- c(candidates[candidates != -1]))
cand_data1 <- data.frame("position" <- c(1:94), "cost_val" <- c(candidates1[1:94]))
sig_data <- data.frame("position" <- c(1:94), "value" <- c(signal[1:94]))
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
for(i in c(49:88)){
  mean1 <- mean(signal[1:prev_break])
  mean2 <- mean(signal[prev_break:i])
  mean3 <- mean(signal[i:94])

  segments <- data.frame("start" <- c(1,prev_break+0.5,i+0.5), "end" <- c(prev_break+0.5,i+0.5, 94), "mean" <- c(mean1, mean2, mean3))

  plot <- ggplot() + geom_point(aes(position, cost_val, color=model), data=add.x.var(comb_data, "cost")) +
    geom_point(aes(position, value), data=add.x.var(sig_data, "signal"))+
    penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),                                                      color="pink",fill="pink",data=add.x.var(one_labels, "signal")) +
    penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),                                                      color="yellow",fill="yellow",data=add.x.var(zero_labels, "signal")) +
    geom_vline(aes(
      xintercept=(i + 0.5)),
      data=positions,
      color="green",
      size=0.7,
      linetype="dashed")+
    geom_vline(aes(
      xintercept=(prev_break + 0.5)),
      data=add.x.var(positions,"signal"),
      color="green",
      size=0.7,
      linetype="dashed")+
    geom_segment(aes(x=start, xend=end, y=mean, yend=mean), col=I("green"),
                 data=add.x.var(segments, "signal"))+
    facet_grid(x.var ~ ., scales="free")
  print(plot)
}

