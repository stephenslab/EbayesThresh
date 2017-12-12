\name{ebayesthresh}
\alias{ebayesthresh}
\title{Empirical Bayes thresholding on a sequence}
\description{
Given a sequence of data, performs Empirical Bayes thresholding, 
as discussed in Johnstone and Silverman (2004). 
}
\usage{
ebayesthresh(x, prior = "laplace", a = 0.5, bayesfac = FALSE, 
sdev = NA, verbose = FALSE, threshrule = "median", universalthresh = TRUE,
stabadjustment)
}
\arguments{
 \item{x}{Vector of data values.}
 
 \item{prior}{Specification of prior to be used conditional on the
	mean being nonzero; can be \code{"cauchy"} or \code{"laplace"}.}
 
 \item{a}{Inverse scale (\emph{i.e.}, rate) parameter if Laplace prior
   is used. Ignored if Cauchy prior is used. If, on entry, \code{a = NA}
   and \code{prior = "laplace"}, then the inverse scale parameter will
   also be estimated by marginal maximum likelihood. If \code{a} is not
   specified then the default value 0.5 will be used.}
 
 \item{bayesfac}{If \code{bayesfac = TRUE}, then whenever a threshold
	is explicitly calculated, the Bayes factor threshold will be
	used.}
 
 \item{sdev}{The sampling standard deviation of the data \code{x}. If,
	on entry, \code{sdev = NA}, then the standard deviation will be
	estimated using the median absolute deviation from zero, as
	\code{mad(x, center = 0)}. If a single value is passed to
	\code{sdev}, sampling standard deviation will be the same for
	all observations. A vector of the same length as data sequence
	can be passed to allow heterogeneous standard deviation,
	currently only for Laplace prior.}
 
 \item{verbose}{Controls the level of output. See below.}
 
 \item{threshrule}{Specifies the thresholding rule to be applied to the
	data. Possible values are \code{"median"} (use the posterior
	median); \code{"mean"} (use the posterior mean); \code{"hard"}
	(carry out hard thresholding); \code{"soft"} (carry out soft
	thresholding); \code{"none"} (find various parameters, but do
	not carry out any thresholding).}
 
\item{universalthresh}{If \code{universalthresh = TRUE}, the
	thresholds will be upper bounded by universal threshold;
	otherwise, the thresholds can take any non-negative values. }
 
\item{stabadjustment}{If
	\code{stabadjustment = TRUE}, the vectors of standard deviations
	and data values will be divided by the mean of standard
	deviations in case of inefficiency caused by large value of
	standard deviation. For heterogeneous sampling standard deviation only;
	ignored if standard deviation is homogeneous.}
}
\details{
It is assumed that the data vector \eqn{(x_1, \ldots, x_n)} is such that
each \eqn{x_i} is drawn independently from a normal distribution with
mean \eqn{\theta_i} and variance \eqn{\sigma_i^2} (\eqn{\sigma_i} is the
same in the homogeneous case). The prior distribution of each
\eqn{\theta_i} is a mixture with probability \eqn{1-w} of zero and
probability \eqn{w} of a given symmetric heavy-tailed distribution. The
mixing weight, and optionally an inverse scale parameter in the symmetric
distribution, are estimated by marginal maximum likelihood. The
resulting values are used as the hyperparameters in the prior.

The true effects \eqn{\theta_i} can be estimated as the posterior median
or the posterior mean given the data, or by hard or soft thresholding
using the posterior median threshold. If hard or soft thresholding is
chosen, then there is the additional choice of using the Bayes factor
threshold, which is the value such thatthe posterior probability of zero
is exactly half if the data value is equal to the threshold.
}
\value{

If \code{verbose = FALSE}, a vector giving the values of the estimates
of the underlying mean vector.

If \code{verbose = TRUE}, a list with the following elements:

  \item{muhat}{the estimated mean vector (omitted if \code{threshrule =
    "none"})}
 
  \item{x}{the data vector as supplied}
 
  \item{threshold.sdevscale}{the threshold as a multiple of the standard
    deviation \code{sdev}}
 
  \item{threshold.origscale}{ the threshold measured on the original
    scale of the data}

  \item{prior}{the prior that was used}

  \item{w}{the mixing weight as estimated by marginal maximum
    likelihood}

  \item{a}{(only present if Laplace prior used) the inverse scale (or
    rate) parameter as supplied or estimated}

  \item{bayesfac}{the value of the parameter \code{bayesfac},
    determining whether Bayes factor or posterior median thresholds are
    used}

  \item{sdev}{the standard deviations of the data as supplied or
    estimated}
  
  \item{threshrule}{the thresholding rule used, as specified above}
}
\references{
  
Johnstone, I. M. and Silverman, B. W. (2004) Needles and straw in
haystacks: Empirical Bayes estimates of possibly sparse
sequences. \emph{Annals of Statistics}, \bold{32}, 1594--1649.

Johnstone, I. M. and Silverman, B. W. (2004) EbayesThresh: R software
for Empirical Bayes thresholding. \emph{Journal of Statistical
Software}, \bold{12}.

Johnstone, I. M. (2004) `Function Estimation and Classical Normal
Theory' `The Threshold Selection Problem'. The Wald Lectures I and II,
2004. Available from \url{http://www-stat.stanford.edu/~imj}.

Johnstone, I. M. and Silverman, B. W. (2005) Empirical Bayes selection
of wavelet thresholds. \emph{Annals of Statistics}, \bold{33},
1700--1752.

The papers by Johnstone and Silverman are available from
\url{http://www.bernardsilverman.com}.

See also \url{http://www-stat.stanford.edu/~imj} for further
references, including the draft of a monograph by I. M. Johnstone.

}
\author{Bernard Silverman}
\seealso{\code{\link{tfromx}}, \code{\link{threshld}}}
\examples{

# Data with homogeneous sampling standard deviation using 
# Cauchy prior.
eb1 <- ebayesthresh(x = rnorm(100, c(rep(0,90),rep(5,10))),
                     prior = "cauchy", sdev = NA)

# Data with homogeneous sampling standard deviation using 
# Laplace prior.
eb2 <- ebayesthresh(x = rnorm(100, c(rep(0,90), rep(5,10))),
                     prior = "laplace", sdev = 1)
             
# Data with heterogeneous sampling standard deviation using 
# Laplace prior.
set.seed(123)
mu <- c(rep(0,90), rep(5,10))
sd <- c(rep(1, 40), rep(3, 60))
x  <- mu + rnorm(100, sd = sd)

# With constraints on thresholds.
eb3 <- ebayesthresh(x = x, prior = "laplace", a = NA, sdev = sd)

# Without constraints on thresholds. Observe that the estimates with
# constraints on thresholds have fewer zeroes than the estimates without
# constraints.
eb4 <- ebayesthresh(x = x, prior = "laplace", a = NA, sdev = sd,
                     universalthresh = FALSE)
print(sum(eb3 == 0))
print(sum(eb4 == 0))

# Data with heterogeneous sampling standard deviation using Laplace
# prior.
set.seed(123)
mu <- c(rep(0,90), rep(5,10))
sd <- c(rep(1, 40), rep(5,40), rep(15, 20))
x  <- mu + rnorm(100, sd = sd)

# In this example, infinity is returned as estimate when some of the
# sampling standard deviations are extremely large. However, this can
# be solved by stabilizing the data sequence before the analysis.
eb5 <- ebayesthresh(x = x, prior = "laplace", a = NA, sdev = sd)

# With stabilization.
eb6 <- ebayesthresh(x = x, prior = "laplace", a = NA, sdev = sd,
                    stabadjustment = TRUE)

}
\keyword{nonparametric}
