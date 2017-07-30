\name{postmed}
\alias{postmed}
\alias{postmed.laplace}
\alias{postmed.cauchy}
\alias{cauchy.medzero}
\title{Posterior median estimator}
\description{
  Given a single value or a vector of data and sampling standard
  deviations (sd is 1 for Cauchy prior), find the corresponding
  posterior median estimate(s) of the underlying signal value(s).
}
\usage{
postmed(x, s, w = 0.5, prior = "laplace", a = 0.5)
postmed.laplace(x, s = 1, w = 0.5, a = 0.5)
postmed.cauchy(x, w)
cauchy.medzero(x, z, w)
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

  \item{z is the data vector (or scalar) provided as input to
    \code{cauchy.medzero}.}
}
\value{
If \eqn{x} is a scalar, the posterior median
\eqn{\mbox{med}(\theta|x)}{med(theta|x)} where \eqn{\theta}{theta} is
the mean of the distribution from which \eqn{x} is drawn.  If \eqn{x} is
a vector with elements \eqn{x_1, ... , x_n} and \eqn{s} is a vector with
elements \eqn{s_1, ... , s_n} (s_i is 1 for Cauchy prior), then the
vector returned has elements \eqn{\mbox{med}(\theta_i|x_i,
s_i)}{med(theta_i|x_i, s_i)}, where each \eqn{x_i} has mean
\eqn{\theta_i}{theta_i} and standard deviation \eqn{s_i}, all with the
given prior.
}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{If the quasicauchy prior is used, the argument \code{a} and
\code{s} are ignored. The routine calls the approprate one of
\code{postmed.laplace} or \code{postmed.cauchy}.
}
\details{The routine calls the relevant one of the routines
\code{postmed.laplace} or \code{postmed.cauchy}. In the Laplace case,
the posterior median is found explicitly, without any need for the
numerical solution of an equation. In the quasi-Cauchy case, the
posterior median is found by finding the zero, component by component,
of the vector function \code{cauchy.medzero}.
}
\seealso{
\code{\link{postmean}}}
\examples{
postmed(c(-2,1,0,-4,8,50), w = 0.05, prior = "cauchy")
postmed(c(-2,1,0,-4,8,50), s = 1:6, w = 0.2, prior = "laplace", a = 0.3)
}
\keyword{nonparametric}
