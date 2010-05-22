plot.fdt.default <-
function (x, type=c('h', 'f', 'rf', 'rfp', 'd', 'cd', 'cf', 'cfp'),
  xlab='Class limits', ylab=NULL, col='gray',
  xlim=NULL, ylim=NULL, main=NULL, x.round=2, x.las=1, ...)
{
  breaks <- with(x, seq(breaks['start'], breaks['end'], breaks['h']))

  if (is.null(xlim))
    xlim <- with(x, c(breaks['start'], breaks['end']))

  switch(match.arg(type),
    # Absolut frequency - histogram
    h = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 2])))

      if(is.null(ylab))
        ylab <- 'Frequency'

      plot.new()
      plot.window(xlim, ylim)
      title(main=main, xlab=xlab, ylab=ylab, ...)
      axis(2, ...)
      rect(breaks[-length(breaks)], 0, breaks[-1], x$table[, 2],
           col=col, ...)
    },
    
    # Absolut frequency - poligon
    f = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 2])))

      if(is.null(ylab))
        ylab <- 'Frequency'

      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 2])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },
    
    # Relative frequency - poligon
    rf = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 3])))

      if(is.null(ylab))
        ylab <- 'Frequency'

      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 3])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Relative frequency (%) - poligon
    rfp = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 4])))

      if(is.null(ylab))
        ylab <- 'Frequency'

      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      y    <- with(x, table[, 4])
      plot(mids, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Density
    d = {
      h <- with(x, breaks[3])
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * max(table[, 3]/h)))

      if(is.null(ylab))
        ylab <- 'Density'

      plot.new()
      plot.window(xlim, ylim)
      title(main=main, xlab=xlab, ylab=ylab, ...)
      axis(2, ...)
      y <- x$table[, 3]/h
      rect(breaks[-length(breaks)], 0, breaks[-1], y,
           col=col, ...)
    },  

    # Cumulative density
    cd = {
      h <- with(x, breaks[3])
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2))

      if(is.null(ylab))
        ylab <- 'Cumulative density'

      y <- c(0, cumsum(x$table[, 3]))
      plot(breaks, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    }, 
    
    # Cumulative frequency - poligon
    cf = {
      if (is.null(ylim))
        ylim <- with(x, c(0, 1.2 * sum(table['f'])))

      if(is.null(ylab))
        ylab <- 'Frequency'

      y <- with(x, c(0, table[, 5]))
      plot(breaks, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    },

    # Cumulative frequency (%) - poligon
    cfp = {
      if (is.null(ylim))
        ylim <- c(0, 1.2 * 100)

      if(is.null(ylab))
        ylab <- 'Frequency'

      y <- with(x, c(0, table[, 6]))
      plot(breaks, y, type='b', xaxt='n', bty='n',
           xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           col=col, main=main, ...)
    })

  axis(1, at=round(breaks, x.round), las=x.las, ...)
}
