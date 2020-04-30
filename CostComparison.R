library(ggplot2)
library(data.table)
library(tikzDevice)
library(penaltyLearning)
library(latex2exp)
library(opart)
library(microbenchmark)
library(directlabels)

signal <- c(rnorm(25, mean = 10), rnorm(25, mean = 5), rnorm(25, mean = 10), rnorm(25, mean = 5))
position <- c(1:100)
selData <- as.data.frame(cbind(position, signal))
labels <- data.frame("start" = c(24, 49, 84), "end" = c(29, 54, 89), "breaks" = c(1, 1, 0))
z1 = signal[1]
zn = signal[100]

res_opart <- opart::opart_gaussian(signal, penalty=0)
cost_opart <- res_opart$cost.vec
labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, 0)
cost_labelled <- labelled_fit$cost.vec

to_plot <- data.frame("time" = c(c(1:100), c(1:100)),
                      "cost" = c(cost_opart, cost_labelled),
                      "type" = c(rep("opart", 100), rep("labelled_opart", 100)))
ggplot() + geom_point(aes(x=time,y=cost,color=type), data=to_plot)
