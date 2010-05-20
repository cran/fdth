plot.fdt.default <-
function (x, type=c('h', 'fp', 'rfp', 'rfpp', 'cfp', 'cfpp'),
  freq=TRUE, xlab='Class limits', ylab=ifelse(freq, 'Frequency', 'Density'),
  col='gray', xlim=NULL, ylim=NULL, main=NULL, x.round=2, x.las=1, ...)
{
  breaks <- with(x, seq(breaks['start'], breaks['end'], breaks['h']))

  if (is.null(xlim))
    xlim <- with(x, c(breaks['start'], breaks['end']))

  switch(match.arg(type),
    # Histogram
    h = {
      if (freq & (is.null(ylim)))
        ylim <- with(x, c(0, 1.2 * max(table[, 2])))
      right <- with(x, as.logical(breaks['right']))
      hist(x$data, breaks=breaks, freq=freq, right=right, xlab=xlab, ylab=ylab,
           col=col, xlim=xlim, ylim=ylim, xaxt='n', main=main, ...)
    },
    
    # Frequency poligon
    fp = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 2])))
      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 2])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },
    
    # Relative frequency poligon
    rfp = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 3])))
      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 3])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Relative frequency poligon, %
    rfpp = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 4])))
      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 4])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Cumlative frequency poligon
    cfp = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * length(data)))
      y <- with(x, c(0, table[, 5]))
      plot(breaks, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Cumulative frequency poligon, %
    cfpp = {
      if (is.null(ylim))
        ylim <- c(0, 1.2 * 100)
      y <- with(x, c(0, table[, 6]))
      plot(breaks, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    })

  axis(1, at=round(breaks, x.round), las=x.las, ...)
}
