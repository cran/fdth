### R code from vignette source 'latex_fdt.Rnw'

###################################################
### code chunk number 1: tab
###################################################
library(fdth)
library(xtable)

t1 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2),
          x.round=3)

t1x <- xtable(t1)
t1x


###################################################
### code chunk number 2: latex_fdt.Rnw:87-90
###################################################
print(t1x,
      include.rownames=FALSE,
      sanitize.text.function = function(x){x})


###################################################
### code chunk number 3: latex_fdt.Rnw:97-110
###################################################
newclass <- gsub("[$\\\\[\\\\)$]",
                 "",
                 t1x[,1],
                 perl=TRUE)
t3x <- t1x
t3x[,1] <- newclass

print(t3x,
      include.rownames=FALSE,
      sanitize.text.function = function(x)gsub(",",
                                               "$\\\\dashv$",
                                               x),
      table.placement='H')


###################################################
### code chunk number 4: latex_fdt.Rnw:115-134
###################################################
clim <- t1$table[1]
clim1 <- sapply(clim,
                as.character)
right <- t1$breaks[4]
pattern='%05.2f'

clim2 <- make.fdt.format.classes(clim1,
                                 right,
                                 pattern)
clim3 <- sapply(clim2,
                function(x) paste0("$",
                                   x,
                                   "$"))
t4x <- t1x
t4x[,1] <- clim3

print(t4x,
      include.rownames=FALSE,
      sanitize.text.function = function(x){x})


###################################################
### code chunk number 5: latex_fdt.Rnw:139-145
###################################################
t5 <- fdt(iris[, c(1:2, 5)],
          by='Species')
attr(t5, "subheadings") <- paste0("Variable = ",
                                  names(t5))
print(xtable(t5),
     table.placement='H')


###################################################
### code chunk number 6: latex_fdt.Rnw:150-157
###################################################
t51 <- xtable(t5)
print(t51,
      table.placement='H',
      include.rownames=FALSE,
      sanitize.text.function = function(x){x},
      tabular.environment='longtable',
      floating=FALSE)


###################################################
### code chunk number 7: latex_fdt.Rnw:162-185
###################################################
t6 <- fdt_cat(sample(LETTERS[1:3], 
                     replace=TRUE,
                     size=30))

t6x <- xtable(t6)
print(t6x,
      table.placement='H',
      include.rownames = FALSE)
      

t61 <- fdt_cat(data.frame(c1=sample(LETTERS[1:3],
                                    replace=TRUE,
                                    size=10),
                          c2=sample(letters[4:5],
                                    replace=TRUE,
                                    size=10),
                          stringsAsFactors=TRUE))

t61x <- xtable(t61)
print(t61x,
      table.placement='H',
      include.rownames = FALSE)



###################################################
### code chunk number 8: latex_fdt.Rnw:190-207
###################################################
portugueseT <- c("Intervalo de classes",
                 "f",
                 "fr",
                 "fr(%)",
                 "fa",
                 "fa(%)")
t7 <- t1$table
names(t7) <- portugueseT
t71 <- list(table=t7,
            breaks=t1$breaks)
class(t71) <- "fdt"
t7x <- xtable(t71)

print(t7x,
      table.placement='H',
      include.rownames=FALSE,
      sanitize.text.function = function(x){x})


