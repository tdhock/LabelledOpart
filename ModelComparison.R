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

plot <- ggplot() + geom_point(aes(position, signal), data = selData, shape = 1)
plot <- plot + penaltyLearning::geom_tallrect(aes(xmin=start,xmax=end),
                                              color="pink",fill="pink",data=labels) +
  geom_text(aes(x=-1,y=z1,label=TeX("$z_1$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=102,y=zn,label=TeX("$z_n$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=20,y=8,label=TeX("$\\underline{p_1}=25$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=35,y=8,label=TeX("$\\bar{p_1}=30$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=27.5, y=8.5,label=TeX("$c_1=1$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=45,y=8,label=TeX("$\\underline{p_2}=50$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=60,y=8,label=TeX("$\\bar{p_2}=55$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=52.5,y=8.5,label=TeX("$c_2=1$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=80,y=6,label=TeX("$\\underline{p_3}=85$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=95,y=6,label=TeX("$\\bar{p_3}=90$",output="character")),
            parse=TRUE) +
  geom_text(aes(x=87.5,y=6.5,label=TeX("$c_3=0$",output="character")),
            parse=TRUE) +
  scale_x_continuous(breaks=seq(0,100,by=5))


#for zero penalty
res_opart <- opart::opart_gaussian(signal, penalty=0)
cost_opart <- res_opart$cost.vec
labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, 0)
cost_labelled <- labelled_fit$cost.vec


positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
positions <- as.data.frame("position" <- c(positions[,1 ] + 0.5))
positions_opart <- data.frame("position_op" <- c(res_opart$end.vec[1:length(res_opart$end.vec)-1]))

labelled_plot <- plot + geom_vline(aes(
  xintercept=position),
  data=positions,
  color="green",
  size=1,
  linetype="dashed")
print(labelled_plot)

opart_plot <- plot + geom_vline(aes(
  xintercept=position_op),
  data=positions_opart,
  color="green",
  size=1,
  linetype="dashed")
print(opart_plot)


#for infinite penalty
res_opart <- opart::opart_gaussian(signal, penalty=10000000)
cost_opart <- res_opart$cost.vec
labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, 10000000)
cost_labelled <- labelled_fit$cost.vec


positions <- data.frame("position" <- c(labelled_fit$end.vec[1:length(labelled_fit$end.vec)-1]))
positions <- as.data.frame("position" <- c(positions[,1 ] + 0.5))
positions_opart <- data.frame("position_op" <- c(res_opart$end.vec[1:length(res_opart$end.vec)-1]))

labelled_plot <- plot + geom_vline(aes(
  xintercept=position),
  data=positions,
  color="green",
  size=1,
  linetype="dashed")
print(labelled_plot)

opart_plot <- plot + geom_vline(aes(
  xintercept=position_op),
  data=positions_opart,
  color="green",
  size=1,
  linetype="dashed")
print(opart_plot)

