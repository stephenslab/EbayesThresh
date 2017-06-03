install.packages("EbayesThresh")
library(EbayesThresh)
options(digits=12)

printres <- function(par_vec){
  list_par = as.list(par_vec)
  return(do.call(paste, c(list_par, sep=",")))
}
# beta.laplace
x <- c(-2,1,0,-4,5)
printres(beta.laplace(x))
# postmean 
x <- c(-2,1,0,-4,5)
printres(postmean(x, w = 0.5))
# postmed
x <- c(-2,1,0,-4,5)
printres(postmed(x, w = 0.5))
# tfromw
w = seq(0, 0.8, 0.2)
printres(tfromw(w))
# tfromx
set.seed(123)
x = rnorm(100)
printres(tfromx(x))
# wfromt
t <- seq(5)
printres(wfromt(t))
# wfromx
set.seed(123)
x = rnorm(100)
printres(wfromx(x))
# wandafromx
set.seed(123)
x = rnorm(100)
printres(wandafromx(x))
# ebayesthresh, 1st test
set.seed(123)
mu = c(rep(0, 50), rnorm(50, sd=2))
x = mu + rnorm(100)
printres(ebayesthresh(x, sdev=1))
# ebayesthresh, 2nd test
set.seed(120)
mu = c(rep(0, 50), rnorm(50, sd=2))
x = mu + rnorm(100)
printres(ebayesthresh(x, sdev=1))
# ebayesthresh, 3rd test
set.seed(120)
mu = c(rep(0, 50), rnorm(50, sd=2))
x = mu + rnorm(100)
printres(ebayesthresh(x, sdev=1))

library(devtools)
install_github("stephenslab/EbayesThresh", ref = "ET_Upgrade_Apr_1st")
library(EbayesThresh)
# ebayesthresh, 4th test
set.seed(120)
mu = c(rep(0, 25), rnorm(25, sd=2))
s = rchisq(50, 1)
x = mu + rnorm(50, sd=s)
printres(ebayesthresh(x, sdev=s))