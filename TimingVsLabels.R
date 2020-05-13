library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)

'
labels <- data.frame("start" = c(), "end" = c(), "breaks" = c())
timing_list <- list()
start <- 0
end <- 0
for(i in 1:500){
  print(i)
  signal <- rnorm(100010, mean=i)
  size <- i
  start <- i * 200 - 10
  end <- start + 10
  labels <- rbind(labels, c(start, end, 1))
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
    times=3)

  timing_list[[paste(i)]] <- data.table("labels", size, timing)
}
timing.dt <- do.call(rbind, timing_list)
'

timing.dt <- read.csv("vignettes/TimingVsLabels.csv")

lab.df <- data.frame(seconds <- c(1,60),
                     times <- c("1 second", "1 minute"))


timing.dt1 <- read.csv("vignettes/TimingList.csv")
timing.dt1$time <- timing.dt1$time / (10^(9))
timing.dt1$V1 <- "size"

timing.dt2 <- read.csv("vignettes/TimingVsSizeLabels500.csv")
timing.dt2$V1 <- "size(labels=500)"

comb_data <- rbind(timing.dt2, timing.dt1, timing.dt)
comb_data$expr <- as.character(comb_data$expr)
comb_data$expr <- ifelse(comb_data$expr == "labelled_opart", "labelled\nopart", comb_data$expr)

lab.df1 <- lab.df
lab.df$V1 <- "labels"
lab.df1$V1 <- "size"

lab.df2 <- lab.df
lab.df2$V1 <- "size(labels=500)"

png("file.png", width=12, height=7, units="in", res=200)

plt <- ggplot(data = comb_data, aes(x = size, y = time, col = expr)) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df,
             color="grey")+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df1,
             color="grey")+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df2,
             color="grey")+
  geom_text(aes(1, (seconds), label=times),
            data=lab.df,
            size=2,
            color="black",
            vjust=-0.5)+
  geom_text(aes(15, 1000, label="datasize=100000"),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(4000, 1000, label="No. of labels = 5"),
            data=lab.df1,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(4000, 1000, label="No. of labels = 500"),
            data=lab.df2,
            size=3,
            color="black"
            )+
  facet_grid(. ~ V1, scales="free")+
  scale_x_log10("") + scale_y_log10("time(s)")

directlabels::direct.label(plt, "last.polygons")
dev.off()
