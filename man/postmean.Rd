\name{postmean}
\alias{postmean}
\alias{postmean.laplace}
\alias{postmean.cauchy}
\title{Posterior mean estimator}
\description{
Given a data value or a vector of data, find the corresponding posterior mean estimate(s) of the underlying signal value(s)
}
\usage{
postmean(x, w, prior = "laplace", a = 0.5)
}
\arguments{
  \item{x}{ a data value or a vector of data}
  \item{w}{ the value of the prior probability that the signal is nonzero }
  \item{prior}{ family of the nonzero part of the prior; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"} }
  \item{a}{the scale parameter of the nonzero part of the prior if the Laplace prior is used}
}
\value{
If \eqn{x} is a scalar, the posterior mean \eqn{E(\theta|x)}{E(theta|x)} where \eqn{\theta}{theta} is the mean of the distribution from which \eqn{x} is drawn.    If \eqn{x} is a vector with elements \eqn{x_1, ... , x_n}, then the vector returned has elements \eqn{E(\theta_i|x_i)}{E(theta_i|x_i)}, where each \eqn{x_i} has mean \eqn{\theta_i}{theta_i}, all with the given prior.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{If the quasicauchy prior is used, the argument \code{a} is ignored.
If \code{prior="laplace"}, the routine calls \code{postmean.laplace}, which finds the posterior
mean explicitly, as the product of the posterior probability that the parameter is nonzero and
the posterior mean conditional on not being zero.   
If \code{prior="cauchy"}, the routine calls \code{postmean.cauchy}; in that case
the posterior mean is found by expressing the quasi-Cauchy prior as a mixture:
The mean conditional on the mixing parameter is found and is then averaged 
over the posterior distribution of the mixing parameter, 
including the atom of probability at zero variance.
}
\seealso{
\code{\link{postmed}}
}
\examples{
postmean(c(-2,1,0,-4,8,50), w=0.05, prior="cauchy")
postmean(c(-2,1,0,-4,8,50), w=0.2, prior="laplace", a=0.3)
}
\keyword{nonparametric}
