negloglik.laplace <- function(xpar, xx, ss, tlo, thi) {

  #
  #  Marginal negative log likelihood function for laplace prior. 
  #   Constraints for thresholds need to be passed externally.
  #  This function replaces the one (below) implemented in the original package to
  # improve computational stability. This function differs by a constant
  # sum(dnorm(x,0,s,log=TRUE)) from the original, because it returns the "actual"
  # likelihood rather than the likelihood up to a constant
  #
  #  xx   :data
  #  xpar :vector of two parameters:
  #      xpar[1] : a value between [0, 1] which will be adjusted to range of w 
  #      xpar[2] : scale factor a
  #  ss   :vector of standard deviations
  #  tlo  :lower bound of thresholds
  #  thi  :upper bound of thresholds
  #
  a <- xpar[2]
  
  # Calculate the range of w given a, using negative monotonicity
  # between w and t
  wlo <- wfromt(thi, ss, a = a)
  whi <- wfromt(tlo, ss, a = a)
  wlo <- max(wlo)
  whi <- min(whi)
  w = xpar[1] * (whi - wlo) + wlo
  
  return(-loglik.laplace(xx,ss,a,w))
}

# This function is no longer used, but included for potential convenience in future
# if we need to debug new results 
negloglik.laplace.orig <- function(xpar, xx, ss, tlo, thi) {
  #
  #  Marginal negative log likelihood function for laplace prior. 
  #   Constraints for thresholds need to be passed externally.
  #  This is the approach implemented in the original package.
  # However, the actual computation is not very stable, because beta.laplace
  # is not stable for small standard errors (s). This may not impact the optimization greatly (unclear)
  # but for stable accurate log-likelihood computations use loglik.laplace 
  #  xx   :data
  #  xpar :vector of two parameters:
  #      xpar[1] : a value between [0, 1] which will be adjusted to range of w 
  #      xpar[2] : scale factor a
  #  ss   :vector of standard deviations
  #  tlo  :lower bound of thresholds
  #  thi  :upper bound of thresholds
  #
  a <- xpar[2]
  
  # Calculate the range of w given a, using negative monotonicity
  # between w and t
  wlo <- wfromt(thi, ss, a = a)
  whi <- wfromt(tlo, ss, a = a)
  wlo <- max(wlo)
  whi <- min(whi)
  loglik <- sum(log(1 + (xpar[1] * (whi - wlo) + wlo) *
                      beta.laplace(xx, ss, a)))
  return(-loglik)
}
