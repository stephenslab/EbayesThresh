devtools::load_all('/Users/KanXu/Git/EbayesThresh/R')
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
printres(postmean(x, s=1))
# postmed
x <- c(-2,1,0,-4,5)
printres(postmed(x, s=1))
# tfromw
w = seq(0, 0.8, 0.2)
printres(tfromw(w, s=1))
# tfromx
set.seed(123)
x = rnorm(100)
printres(tfromx(x, s=1))
# wfromt
t <- seq(5)
printres(wfromt(t, s=1))
# wfromx
set.seed(123)
x = rnorm(100)
printres(wfromx(x, s=1))
# wandafromx
set.seed(123)
x = rnorm(100)
printres(wandafromx(x, s=1))
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
