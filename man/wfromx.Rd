\name{wfromx}
\alias{wfromx}
\title{Find Empirical Bayes weight from data}
\description{
Suppose the vector \eqn{(x_1, \ldots, x_n)} is such that \eqn{x_i} is drawn independently from 
a normal distribution with mean \eqn{\theta_i}{\theta_i} and variance 1.
The prior distribution of the \eqn{\theta_i}{theta_i} is a mixture with probability \eqn{1-w}
of zero and probability \eqn{w} of a given symmetric heavy-tailed distribution.
This routine finds the marginal maximum likelihood estimate of the parameter \eqn{w}.
}
\usage{
wfromx(x, prior = "laplace", a = 0.5)
}
\arguments{
  \item{x}{vector of data}
  \item{prior}{specification of prior to be used; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"}}
  \item{a}{scale factor if Laplace prior is used.  Ignored if Cauchy prior is used. }
}
\details{
The weight is found by marginal maximum likelihood. The search is over weights corresponding to thresholds 
in the range 
\eqn{[0, \sqrt{2 \log n}]}, 
where \eqn{n} is the length of the data vector.

The search is by binary search for a solution to the equation 
\eqn{S(w)=0}, where \eqn{S} is the derivative of the log likelihood.
The binary search is on a logarithmic scale in \eqn{w}.

If the Laplace prior is used, the scale parameter is fixed at the value given for \code{a}, and
defaults to 0.5 if no value is provided.  To estimate \code{a} as well as \code{w} by marginal
maximum likelihood, use the routine \code{\link{wandafromx}}.
}
\value{
The numerical value of the estimated weight.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wandafromx}}, \code{\link{tfromx}}, \code{\link{tfromw}}, \code{\link{wfromt}}
}
\examples{
wfromx(x=rnorm(100, c( rep(0,90), rep(5,10))), prior="cauchy")
}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
