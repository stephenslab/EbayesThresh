\name{postmean}
\alias{postmean}
\alias{postmean.laplace}
\alias{postmean.cauchy}
\title{Posterior mean estimator}
\description{Given a single value or a vector of data and sampling
standard deviations (sd equals 1 for Cauchy prior), find the
corresponding posterior mean estimate(s) of the underlying signal
value(s).}
\usage{
postmean(x, s, w, prior = "laplace", a = 0.5)
}
\arguments{
  \item{x}{A data value or a vector of data.}
  
  \item{s}{A single value or a vector of standard deviations if the
  Laplace prior is used. If a vector, must have the same length as
  \code{x}. Ignored if Cauchy prior is used.}

  \item{w}{The value of the prior probability that the signal is
  nonzero.}
 
  \item{prior}{Family of the nonzero part of the prior; can be
    \code{"cauchy"} or \code{"laplace"}.}
  
  \item{a}{The scale parameter of the nonzero part of the prior if the
    Laplace prior is used.}
}
\value{
If \eqn{x} is a scalar, the posterior mean \eqn{E(\theta|x)}{E(theta|x)}
where \eqn{\theta}{theta} is the mean of the distribution from which
\eqn{x} is drawn. If \eqn{x} is a vector with elements \eqn{x_1, ... ,
x_n} and \eqn{s} is a vector with elements \eqn{s_1, ... , s_n} (s_i is
1 for Cauchy prior), then the vector returned has elements
\eqn{E(\theta_i|x_i, s_i)}{E(theta_i|x_i, s_i)}, where each \eqn{x_i}
has mean \eqn{\theta_i}{theta_i} and standard deviation \eqn{s_i}, all
with the given prior.
}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{If the quasicauchy prior is used, the argument \code{a} and
  \code{s} are ignored.
  
If \code{prior="laplace"}, the routine calls \code{postmean.laplace},
which finds the posterior mean explicitly, as the product of the
posterior probability that the parameter is nonzero and the posterior
mean conditional on not being zero.

If \code{prior="cauchy"}, the routine calls \code{postmean.cauchy}; in
that case the posterior mean is found by expressing the quasi-Cauchy
prior as a mixture: The mean conditional on the mixing parameter is
found and is then averaged over the posterior distribution of the mixing
parameter, including the atom of probability at zero variance.
}
\seealso{
\code{\link{postmed}}
}
\examples{
postmean(c(-2,1,0,-4,8,50), w = 0.05, prior = "cauchy")
postmean(c(-2,1,0,-4,8,50), s = 1:6, w = 0.2, prior = "laplace", a = 0.3)
}
\keyword{nonparametric}
