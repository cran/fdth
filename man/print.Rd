\name{print.fdt}
\alias{print.fdt}
\alias{print.fdt.default}
\alias{print.fdt.multiple}
\alias{print.fdt_cat.default}
\alias{print.fdt_cat.multiple}

\title{
  Print methods for fdt objects
}

\description{
  S3 methods to return a \code{data.frame} (the frequency distribution table - fdt) 
  for \code{fdt.default} and \code{fdt.multiple} objects; \code{data.frame} (the frequency
  distribution table - fdt_cat) for \code{fdt_cat.default} and \code{fdt_cat.multiple}
  objects.
}

\usage{
## S3 methods
\method{print}{fdt.default}(x,
      columns=1:6,
      round=2,
      format.classes=FALSE,
      pattern='\%09.3e',
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt.multiple}(x,
      columns=1:6,
      round=2,
      format.classes=FALSE,
      pattern='\%09.3e',
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt_cat.default}(x,
      columns=1:6,
      round=2,
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt_cat.multiple}(x,
      columns=1:6,
      round=2,
      row.names=FALSE,
      right=TRUE, \dots)
}

\arguments{
  \item{x}{a \samp{fdt} object.}
  \item{columns}{a \code{vector} of \code{integers} to select columns of the
    \code{data.frame} table.}
  \item{round}{rounds \samp{fdt} columns to the specified number of
    decimal places (default 2).}
  \item{format.classes}{logical, if \code{TRUE} the first column of the
    data.frame table will be formated using regular expression.
    The default is \dQuote{\%09.3e}.}
  \item{pattern}{same as \code{fmt} in \code{\link[base]{sprintf}}.}
  \item{row.names}{logical (or character vector), indicating whether (or what)
    row names should be printed. The default is \code{FALSE}.}
  \item{right}{logical, indicating whether or not strings should be right-aligned.
    The default is right-alignment.}
  \item{\dots}{potential further arguments (require by generic).}
}

\details{
  For \code{print.fdt}, it is possible to select what columns of the table 
  (a \code{data.frame}) will be shown, as well as the pattern of the first column,
   for \code{print.fdt_cat} it is only possible to select what columns of the table 
  (a \code{data.frame}) will be shown. The columns are:
  \enumerate{
    \item \samp{Class limits} 
    \item \samp{f} - absolute frequency
    \item \samp{rf} - relative frequency
    \item \samp{rf(\%)} - relative frequency, \%
    \item \samp{cf} - cumulative frequency
    \item \samp{cf(\%)} - cumulative frequency, \%
  }

  The available parameters offer an easy and powerful way to format the \samp{fdt} for
  publications and other purposes.
}

\value{
  A single \code{data.frame} for \code{fdt.default} and \code{fdt.default} or multiple
  \code{data.frames} for \code{fdt.multiple} and \code{fdt_cat.multiple}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
library (fdth)

#======================
# Vectors: univariated
#======================
set.seed(1)

x <- rnorm(n=1e3,
           mean=5,
           sd=1)

ft <- fdt(x)

str(ft)

ft

print(ft) # the same

print(ft,
      format=TRUE)      # It can not be what you want to publications!

print(ft,
      format=TRUE,
      pattern='\%.2f')   # Huumm ..., good, but ... Can it be better?

print(ft,
      col=c(1:2, 4, 6),
      format=TRUE, 
      pattern='\%.2f')   # Yes, it can!

range(x)                 # To know x

print(fdt(x,
          start=1,
          end=9,
          h=1),
      col=c(1:2, 4, 6),
      format=TRUE,
      pattern='\%d')     # Is it nice now?

ft[['table']]                        # Stores the feq. dist. table (fdt)
ft[['breaks']]                       # Stores the breaks of fdt
ft[['breaks']]['start']              # Stores the left value of the first class
ft[['breaks']]['end']                # Stores the right value of the last class
ft[['breaks']]['h']                  # Stores the class interval
as.logical(ft[['breaks']]['right'])  # Stores the right option

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1=rep(LETTERS[1:4], 25),
                  X2=as.factor(rep(1:10, 10)),
                  Y1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2=rnorm(100, 60, 4),
                  Y3=rnorm(100, 50, 4),
                  Y4=rnorm(100, 40, 4),
                  stringsAsFactors=TRUE)
                  
(ft <- fdt_cat(mdf))
                  
print(ft)

(ft <- fdt(mdf,
         na.rm=TRUE))

print(ft)

str(ft)

print(ft, # the s
      format=TRUE)

print(ft,
      format=TRUE,
      pattern='\%05.2f')  # regular expression

print(ft,
      col=c(1:2, 4, 6), 
      format=TRUE, 
      pattern='\%05.2f')

print(ft,
      col=c(1:2, 4, 6))

print(ft,
      col=c(1:2, 4, 6),
      format=TRUE,
      pattern='\%05.2f')

levels(mdf$X1)

print(fdt(mdf,
          k=5,
          by='X1',
         na.rm=TRUE))

levels(mdf$X2)

print(fdt(mdf,
          breaks='FD',
          by='X2',
          na.rm=TRUE),
      round=3)

print(fdt(mdf,
          k=5,
          by='X2',
          na.rm=TRUE),
      format=TRUE,
      round=3)

print(fdt(iris,
          k=5),
      format=TRUE,
      patter='\%04.2f')

levels(iris$Species)

print(fdt(iris,
          k=5,
          by='Species'),
      format=TRUE, 
      patter='\%04.2f')

#=========================
# Matrices: multivariated
#=========================
print(fdt(state.x77), 
      col=c(1:2, 4, 6),
      format=TRUE)

print(fdt(volcano,
          right=TRUE),
      col=c(1:2, 4, 6),
      round=3,
      format=TRUE,
      pattern='\%05.1f')
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{print}
