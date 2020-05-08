library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)

'
labels <- data.frame("start" = c(), "end" = c(), "breaks" = c())
size <- 1000
timing_list <- list()
start <- 0
end <- 0
for(i in 1:300){
  signal <- rnorm(10000, mean=i)
  size <- i
  start <- i * 30 - 10
  end <- start + 10
  labels <- rbind(labels, c(start, end, round(runif(1))))
  timing <- microbenchmark(
    "labelled_opart"={
      LabelledOpart::labelled_opart_gaussian(signal, labels, 5)
    },
    "fpop"={
      if(requireNamespace("fpop", quietly = TRUE))
        fpop::Fpop(signal, 5)
    },
    "opart"={
      opart::opart_gaussian(signal, 5)
    },
    times=5)

  timing_list[[paste(i)]] <- data.table("labels", size, timing)
}
timing.dt <- do.call(rbind, timing_list)
'

timing.dt <- read.csv("vignettes/TimingVsLabels.csv")
timing.dt$time <- timing.dt$time * (10^(-9))
lab.df <- data.frame(seconds <- c(1,60),
                     times <- c("1 second", "1 minute"))

gg_runtime <- ggplot(data = timing.dt, aes(x = size, y = time, col = expr)) +
  geom_point() +
  geom_smooth() +

  geom_hline(aes(yintercept=(seconds)),
             data=lab.df,
             color="grey")+
  geom_text(aes(5, (seconds), label=times),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  scale_x_log10("number of labels") + scale_y_log10("time(s)")
print(gg_runtime)

timing.dt1 <- read.csv("vignettes/TimingList.csv")
timing.dt1$time <- timing.dt1$time * (10^(-9))
timing.dt1$V1 <- "size"

comb_data <- rbind(timing.dt1, timing.dt)
lab.df1 <- lab.df
lab.df$V1 <- "labels"
lab.df1$V1 <- "size"

ggplot(data = comb_data, aes(x = size, y = time, col = expr)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(ylim=c(min(comb_data$time), max(comb_data$time)))+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df,
             color="grey")+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df1,
             color="grey")+
  geom_text(aes(1, (seconds), label=times),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(5, 1000, label="datasize=100000"),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(4000, 1000, label="No. of labels = 5"),
            data=lab.df1,
            size=3,
            color="black",
            vjust=-0.5)+
  facet_grid(. ~ V1, scales="free")+
  scale_x_log10("") + scale_y_log10("time(s)")
