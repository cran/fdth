summary.fdt.default <-
function (object, columns=1:6, round=2, format.classes=FALSE,
  pattern='%09.3e', ...)
{
  res   <- object[['table']]
  res   <- cbind(res[, 1], round(res[, 2:6], round))[columns]
  right <- as.logical(object[['breaks']]['right'])
  if (format.classes) {
    tmp      <- as.character(res[, 1])
    res[, 1] <- make.fdt.format.classes(tmp, right, pattern)
  }
  names(res) <- c('Class limits', 'f', 'rf', 'rf(%)', 'cf', 'cf(%)')[columns]
  print(res, row.names=FALSE)
}

