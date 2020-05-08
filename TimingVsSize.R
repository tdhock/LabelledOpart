library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)


# un-comment this code only if you need to create fresh timing list
# the code loads the vignettes/TimingList.csv file which already has timing data

'
timing_list <- list()
cnt = 1

for(p in pid_chrs){
  signal_file <- paste("exp_data/", p, "_signal.csv", sep="")
  labels_file <- paste("exp_data/", p, "_labels.csv", sep="")
  data <- as.data.table(read.csv(file=signal_file))
  regions <- as.data.table(read.csv(file=labels_file))
  signal <- data$logratio
  signal <- as.data.frame(na.omit(signal))
  names(signal) <- c("logratio")
  size <- nrow(signal)

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

  timing <- microbenchmark(
    "labelled_opart"={
      LabelledOpart::labelled_opart_gaussian(signal$logratio, labels, 2)
    },
    "opart"={
      opart::opart_gaussian(signal$logratio, 2)
    },
    "fpop"={
      fpop::Fpop(signal$logratio, 2)
    },
    times=3)

  timing_list[[paste(cnt)]] <- data.table("labels", size, timing)
  cnt <- cnt + 1
}
timing.dt <- do.call(rbind, timing_list)
'

#comment this line if creating fresh timing list
timing.dt <- read.csv("vignettes/TimingList.csv")

timing.dt$time <- timing.dt$time * (10^(-9))
timing.dt$expr <- as.character(timing.dt$expr)
timing.dt$expr <- ifelse(timing.dt$expr == "labelled_opart", "labelled\nopart", timing.dt$expr)


timing.dt.fpop <- timing.dt[timing.dt$expr=="fpop",]
timing.dt.fpop$median <- median(timing.dt.fpop$time)
timing.dt.fpop$q25 <- quantile(timing.dt.fpop$time, 0.25)
timing.dt.fpop$q75 <- quantile(timing.dt.fpop$time, 0.75)

timing.dt.opart <- timing.dt[timing.dt$expr=="opart",]
timing.dt.opart$median <- median(timing.dt.opart$time)
timing.dt.opart$q25 <- quantile(timing.dt.opart$time, 0.25)
timing.dt.opart$q75 <- quantile(timing.dt.opart$time, 0.75)

timing.dt.lopart <- timing.dt[timing.dt$expr=="labelled\nopart",]
timing.dt.lopart$median <- median(timing.dt.lopart$time)
timing.dt.lopart$q25 <- quantile(timing.dt.lopart$time, 0.25)
timing.dt.lopart$q75 <- quantile(timing.dt.lopart$time, 0.75)

combined <- rbind(timing.dt.opart, timing.dt.lopart, timing.dt.fpop)

lab.df <- data.frame(seconds <- c(1,60),
                     times <- c("1 second", "1 minute"))

gg_runtime <- ggplot(data = combined, aes(x = size, y = time, col = expr)) +
  geom_point() +
  geom_ribbon(aes(ymin = time - q25, ymax = time + q25),
              alpha = .25, fill="red")+


  geom_hline(aes(yintercept=(seconds)),
             data=lab.df,
             color="grey")+
  geom_text(aes(min(timing.dt$size) + 10, seconds, label=times),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  coord_cartesian(xlim=c(min(timing.dt$size), max(timing.dt$size)))+
  scale_x_log10("log10(data size)") + scale_y_log10("log10(time(s))")

directlabels::direct.label(gg_runtime, "last.polygons")
