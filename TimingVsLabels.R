library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)


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


lab.df <- data.frame(seconds <- c(1000000000,60000000000,3600000000000),
                     times <- c("1 second", "1 minute", "1 hour"))

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
