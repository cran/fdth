plot.fdt.multiple <-
function (x, type=c('h', 'fp', 'rfp', 'rfpp', 'cfp', 'cfpp'),
  freq=TRUE, xlab='Class limits', ylab=ifelse(freq, 'Frequency', 'Density'),
  col='gray', xlim=NULL, ylim=NULL, main=NULL, x.round=2, x.las=1, ...)
{
  is.wholenumber <- function (x, tol=.Machine$double.eps^0.5)
    abs(x - round(x)) < tol
  
  old.mf  <- par("mfrow")
  old.oma <- par("oma")
  old.mar <- par("mar")
  on.exit(par(mfrow=old.mf, oma=old.oma, mar=old.mar))
  mf <- old.mf
  
  if (length(mf) == 0)
    mf <- c(1, 1)
  
  if ((n <- length(x)) > 1 & max(mf) == 1)
    mf <- if (n <= 2)       c(2, 1)
          else if (n <= 4)  c(2, 2)
          else if (n <= 6)  c(2, 3)
          else if (n <= 9)  c(3, 3)
          else if (n <= 12) c(3, 4)
          else if (n <= 16) c(4, 4)
          else              c(4, 5)
  
  par(mfrow=mf)
  nplot.device <- prod(mf)
  i <- 0
  repeat {
    if ((i != 0) & is.wholenumber(i/nplot.device)) {
        x11()
        par(mfrow=mf)
    }
    
    i <- i + 1
    plot.fdt.default(x[[i]], type=type, freq=freq, xlab=xlab, ylab=ylab, col=col,
      xlim=xlim, ylim=ylim, main=ifelse (is.null(main), '', names(x[i])),
      x.round=x.round, x.las=x.las, ...)
    
    if (i == length(x)) break
  }
}

