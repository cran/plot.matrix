## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.height=4, fig.width=4-------------------------------------------
library('plot.matrix')
# numeric matrix
x <- matrix(runif(35), ncol=5) # create a numeric matrix object
class(x)
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(x)
# logical matrix
m <- matrix(runif(35)<0.5, ncol=7)
plot(m)
# text matrix
s <- matrix(sample(letters[1:10], 35, replace=TRUE), ncol=5)
plot(s)

## ----fig.height=5, fig.width=5-------------------------------------------
library('plot.matrix')
library('psych')
data <- na.omit(bfi[,1:25])
fa <- fa(data, 5, rotate="varimax")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(loadings(fa), cex=0.5)

## ----echo=FALSE----------------------------------------------------------
p   <-formals(plot.matrix:::plot.matrix)
p$na.col <- paste0('"', p$na.col, '"')
out <- matrix('', nrow=length(p), ncol=4)
out[1,1] <- 'plot.matrix('
out[,2] <- names(p)
cp  <- as.character(p)
ct  <- (cp!='')
out[ct,3] <- '=' 
out[ct,4] <- cp[ct] 
out[1:length(p),4] <- cp
out[1,2] <- paste0(out[1,2], ',')
out[2:(length(p)-1),4] <- paste0(out[2:(length(p)-1),4], ',')
out[length(p),2] <- paste0(out[length(p),2], ')')
#out <- cbind(out[,1], apply(out[,2:5], 1, function(e) paste0(e, collapse="")))
knitr::kable(out)

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
# omit all borders
plot(x, border=NA)

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
# we only want the range of x
plot(x, breaks=range(x))     
# we want seven colors
plot(x, breaks=7)  
# user defined breaks, out-of-range entries are colored white
plot(x, breaks=c(0.3,0.5,0.8))  

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
# use a color function
plot(x, col=topo.colors)
# user defined breaks and colors
plot(x, col=c('red', 'green'), breaks=c(0, 0.5, 1))   
# non-numeric matrix
# assign colors
plot(m, col=c('red', 'green'))
# assign colors and breaks directly 
plot(m, col=c('red', 'green'), breaks=c(TRUE, FALSE))

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
x[1,1] <- NA
plot(x, col=topo.colors)
plot(x, col=topo.colors, na.col="red")
plot(s, col=topo.colors, breaks=c('a', 'c', 'e', 'g', 'i'))

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
# delete key
plot(x, key=NULL)

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 5.1, 4.1))   # adapt margins
# move key to the top and make axis text smaller
plot(x, key=list(side=3, cex.axis=0.75), breaks=c(0,1))

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
# no plus sign
plot(x, fmt.key="%.3f", breaks=c(0,1))

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
plot(x, xlab="my x label", ylab="my y label")
# The HairEyeColor has its own names
tab <- apply(HairEyeColor,1:2, sum)
plot(tab)

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 5.1, 4.1))   # adapt margins
plot(x, axis.col=list(side=3, cex.axis=0.7), axis.row=list(cex.axis=0.7))
# or alternatively set cex.axis for all axes and use abbreviated positioning
plot(x, axis.col=3, cex.axis=0.7)

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
plot(x, axis.col=NULL, axis.row=NULL, xlab='', ylab='')

## ----fig.height=4, fig.width=4-------------------------------------------
par(mar=c(5.1, 4.1, 4.1, 4.1))   # adapt margins
# change all text output
plot(x, digits=4, text.cell=list(cex=0.5))
# of alternatively use the global parameter cex
plot(x, digits=4, cex=0.5)
# change just matrix entries (no plus sign)
plot(x, fmt.cell='%.2f')

## ----fig.height=5, fig.width=5-------------------------------------------
library('plot.matrix')
library('psych')
data(bfi.2)
fa <- fa(bfi.2, 5, rotate="varimax")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(loadings(fa), cex=0.5)

## ----fig.height=5, fig.width=5-------------------------------------------
library('plot.matrix')
library('psych')
data(bfi.2)
fa <- fa(bfi.2, 5, rotate="varimax")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(loadings(fa), digits=NA)

## ----fig.height=5, fig.width=5-------------------------------------------
library('plot.matrix')
library('psych')
data(bfi.2)
fa <- fa(bfi.2, 5, rotate="varimax")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(loadings(fa), reorder=FALSE, cex=0.5)

