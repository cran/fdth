\name{mfv}
\alias{mfv}
\alias{mfv.default}
\alias{mfv.fdt}
\alias{mfv.fdt.multiple}
\alias{mfv.fdt_cat}
\alias{mfv.fdt_cat.multiple}

\title{
  Most frequent value (statistical mode) of frequency distribution table (numerical and categorical variable)
}

\description{
  S3 methods for the most frequent value (statistical mode) of a \code{fdt}. \cr 
  Useful to estimate the most frequent value or statistical mode. May also be used, by using a previous \code{fdt}, when the original data vector is not known.
}

\usage{
## S3 generic
mfv(x, \dots)

## S3 methods: numerical and categorical 
\method{mfv}{default}(x, \dots)

\method{mfv}{fdt}(x, \dots)

\method{mfv}{fdt.multiple}(x, \dots)

\method{mfv}{fdt_cat}(x, \dots)

\method{mfv}{fdt_cat.multiple}(x, \dots)
}

\arguments{
  \item{x}{a \code{fdt} or \code{fdt_cat} (simple or multiple) object.}
  \item{\dots}{required to be generic.}
}

\details{\code{mfv.fdt} and \code{mfv.fdt_cat} calculates the most frequent value (mfv) based on a known formula. 
\code{mfv.fdt.multiple} and \code{mfv.fdt_cat.multiple}call respectively \code{mfv.fdt} 
or \code{mfv.fdt_cat}for each variable, that is, each column of the data.frame.
}

\value{\code{mfv.fdt} returns a numeric vector containing the mfv value of the \code{fdt}.
 \code{mean.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the mean value of the \code{fdt} for each variable.
 \code{mfv.fdt_cat} returns a character vector containing the mfv value of the \code{fdt_cat}.
 \code{mean.fdt_cat.multiple} returns a list, where each element is a character vector 
 containing the mfv value of the \code{fdt_cat} for each variable.
}

\seealso{\code{mean.fdt}, \code{median.fdt}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
# Numerical
mdf <- data.frame(x=rnorm(1e2, 
                          20, 
                          2),
                  y=rnorm(1e2, 
                          30,
                          3),
                  z=rnorm(1e2,
                          40,
                          4))

head(mdf)

mfv(mdf$x)   # From vector x
mfv(mdf$y)   # From vector y
mfv(mdf$z)   # From vector z

(ft <- fdt(mdf))

mfv(ft)      # From agruped data in a fdt object


## Categorical
mdf <- data.frame(c1=sample(letters[1:5],
                            1e3,
                            rep=TRUE),
                  c2=sample(letters[6:10],
                            1e3,
                            rep=TRUE),
                  c3=sample(letters[11:21],
                            1e3,
                            rep=TRUE),
                  stringsAsFactors=TRUE)
head(mdf)

mfv(mdf$c1)   # From vector c1
mfv(mdf$c2)   # From vector c2
mfv(mdf$c3)   # From vector c3

(ft <- fdt_cat(mdf))

mfv(ft)      # From agruped data in a fdt object
}   
