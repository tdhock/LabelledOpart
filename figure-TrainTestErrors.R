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

fn.dt <- err.dt[set=="test", .(
  fn=possible.fn[1]
), by=.(problem, test.fold)]

SegAnnot.compare <- err.dt[
  set=="test" & model.name=="LOPART",
  .SD[which.min(errors)],
  by=.(problem, test.fold)]
## segannot always has FN = possible.fn and FP=0, so how often are we
## better on one or both axes?
SegAnnot.compare[, fewer.FN := possible.fn-fn]
SegAnnot.compare[, table(fp, fewer.FN)]
print(xtable::xtable(SegAnnot.compare[, table(more.fp=fp>0, fewer.fn=fewer.FN>0)]))
SegAnnot.compare[, table(fewer.FN-fp)]

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

total.dt <- err.dt[, .(
  total.errors=min(errors)
), by=.(problem, test.fold, model.name, penalty)]
total.min <- total.dt[, .(
  min.errors=min(total.errors)
), by=.(problem, test.fold, model.name)]
total.min.wide <- dcast(
  total.min,
  problem + test.fold ~ model.name,
  value.var="min.errors")
total.min.wide[, diff := OPART-LOPART]
total.min.wide[, .(
  prob.folds=.N
), keyby=.(diff)]

total.min.wide[diff<0]

80/(413*2)
413*2
