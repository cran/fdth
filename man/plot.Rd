\name{plot.fdt}
\alias{plot.fdt}
\alias{plot.fdt.default}
\alias{plot.fdt.multiple}

\title{
  Plot fdt.default and fdt.multiple objects
}

\description{
  S3 methods for \code{fdt.default} and \code{fdt.multiple} objects.
  It is possible to plot histograms and poligons (absolute, relative
  and cumulative).
}

\usage{
  \method{plot}{fdt.default}(x, type=c('fh', 'fp', 'rfh', 'rfp', 'rfph', 'rfpp',
    'd', 'cdh', 'cdp', 'cfh', 'cfp', 'cfph', 'cfpp'),
    xlab="Class limits", ylab=NULL, col="gray", xlim=NULL, ylim=NULL,
    main=NULL, x.round=2, x.las=1, \dots)
  \method{plot}{fdt.multiple}(x, type=c('fh', 'fp', 'rfh', 'rfp', 'rfph', 'rfpp',
    'd', 'cdh', 'cdp', 'cfh', 'cfp', 'cfph', 'cfpp'),
    xlab="Class limits", ylab=NULL, col="gray", xlim=NULL, ylim=NULL,
    main=NULL, x.round=2, x.las=1, \dots)
}

\arguments{
  \item{x}{A \code{fdt} object.}
  \item{type}{The type of the plot.
    \code{fh:} Absolut frequency histogram,
    \code{fp:} Absolut frequency poligon,
    \code{rfh:} Relative frequency histogram,
    \code{rfp:} Relative frequency poligon,
    \code{rfph:} Relative frequency (\%) histogram ,
    \code{rfpp:} Relative frequency (\%) poligon,
    \code{d:} Density,
    \code{cdh:} Cumulative density histogram,
    \code{cdp:} Cumulative density poligon,
    \code{cfh:} Cumulative frequency histogram,
    \code{cfp:} Cumulative frequency poligon,
    \code{cdph} cumulative frequency (\%) histogram,
    \code{cfpp:} cumulative frequency (\%) poligon.}
  \item{xlab}{A label for the x axis.}
  \item{ylab}{A label for the y axis.}
  \item{col}{A \code{vector} of colors.}
  \item{xlim}{The x limits of the plot.}
  \item{ylim}{The y limits of the plot.}
  \item{main}{A title for the plot.}
  \item{x.round}{A numeric value to round the x ticks.}
  \item{x.las}{An integer which controls the orientation of the x axis labels
    (0: parallel to the axes, 1: horizontal, 2: perpendicular to the axes,
    3: vertical)}
  \item{\dots}{Optional plotting parameters.}
}

\details{
 The result is a single histogram or poligon (absolute, relative or cummulative)
 for \code{fdt.default} or a set of histograms or poligon (absolute, relative or
 cummulative) for \code{fdt.multiple} objects.
 Both \code{default and multiple} try to compute the maximum number of histograms
 that will fit on one page, then it draws a matrix of histograms. More than one
 graphical device may be opened to show all histograms.
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
}

\seealso{
  \code{\link[Hmisc]{hist.data.frame}} provided by \code{Hmisc} package.
}

\examples{
library(fdth)

#======================
# Vectors: univariated
#======================
set.seed(1)
x <- rnorm(n=1e3, mean=5, sd=1)
d <- fdt(x); d

# Histograms
plot(d)
plot(d, main='My title')
plot(d, x.round=3, col='darkgreen')
plot(d, x.las=2)
plot(d, x.round=2, x.las=2, xlab=NULL)
plot(d, x.round=2, x.las=2, xlab=NULL, col=rainbow(11))

plot(d, type='fh')
plot(d, type='rfh')
plot(d, type='rfph')
plot(d, type='cdh')
plot(d, type='cfh')
plot(d, type='cfph')

# Poligons
plot(d, type='fp')
plot(d, type='rfp')
plot(d, type='rfpp')
plot(d, type='cdp')
plot(d, type='cfp')
plot(d, type='cfpp') 

# Density
plot(d, type='d') 

# Theoretical curve and fdt
x <- rnorm(1e5, mean=5, sd=1)
plot(fdt(x, k=100), type='d', col=heat.colors(100))
curve(dnorm(x, mean=5, sd=1), col='darkgreen', add=TRUE, lwd=2)

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1 = rep(LETTERS[1:4], 25),
                  X2 = as.factor(rep(1:10, 10)),
                  Y1 = c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2 = rnorm(100, 60, 4),
                  Y3 = rnorm(100, 50, 4),
                  Y4 = rnorm(100, 40, 4))

# Histograms
d <- fdt(mdf); d
plot(d, main=TRUE)
plot(d, col='darkgreen', ylim=c(0, 40), main=TRUE)
plot(d, col=rainbow(8), ylim=c(0, 40), main=TRUE)

plot(d, type='fh')
plot(d, type='rfh')
plot(d, type='rfph')
plot(d, type='cdh')
plot(d, type='cfh')
plot(d, type='cfph')

# Poligons
plot(d, type='fp')
plot(d, type='rfp')
plot(d, type='rfpp')
plot(d, type='cdp')
plot(d, type='cfp')
plot(d, type='cfpp') 

# Density
plot(d, type='d')     

levels(mdf$X1)
plot(fdt(mdf, k=5, by='X1'), ylim=c(0, 12), main=TRUE)

levels(mdf$X2)
plot(fdt(mdf, breaks='FD', by='X2'), main=TRUE)

plot(fdt(mdf, k=5, by='X2'), main=TRUE)               # It is dificult to compare
plot(fdt(mdf, k=5, by='X2'), ylim=c(0, 8), main=TRUE) # Easy

plot(fdt(iris, k=5), main=TRUE)
plot(fdt(iris, k=5), main=TRUE, col=rainbow(5))

d <- fdt(iris, k=10)
plot(d, main=TRUE)
plot(d, type='d', main=TRUE)

require(MASS)
levels(Cars93$Origin)
plot(fdt(Cars93, k=5, by='Origin'), col=heat.colors(5), main=TRUE)

plot(fdt(Cars93, breaks='FD', by='Origin'), main=TRUE)

#=========================
# Matrices: multivariated
#=========================
plot(fdt(state.x77), main=TRUE)

plot(fdt(volcano), main=TRUE)
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{plot}
\keyword{histogram}
