library(data.table)
library(directlabels)
library(ggplot2)

err.dt <- data.table(
  csv=Sys.glob("LOPART/*.csv")
)[, data.table::fread(
  csv,
  colClasses=list(character=5)
), by=csv]
err.dt[model.name=="LOPART" & set=="train", table(errors)]
err.dt[model.name=="LOPART" & set=="train" & 0<errors]

min.dt <- err.dt[, .(
  min.errors=min(errors)
), by=.(problem, test.fold, set, model.name)]
min.wide <- dcast(
  min.dt,
  problem + test.fold + set ~ model.name,
  value.var="min.errors")
min.wide[, diff := OPART-LOPART]
min.wide[, .(
  prob.folds=.N
), keyby=.(set, diff)]

min.wide[diff<0]
