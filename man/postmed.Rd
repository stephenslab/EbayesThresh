\name{postmed}
\alias{postmed}
\alias{postmed.laplace}
\alias{postmed.cauchy}
\alias{cauchy.medzero}
\title{Posterior median estimator}
\description{
  Given a data value or a vector of data, find the corresponding posterior median estimate(s) of the underlying signal value(s)
}
\usage{
postmed(x, w, prior = "laplace", a = 0.5)
}
\arguments{
  \item{x}{a data value or a vector of data}
  \item{w}{the value of the prior probability that the signal is nonzero }
  \item{prior}{family of the nonzero part of the prior; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"} }
  \item{a}{the scale parameter of the nonzero part of the prior if the Laplace prior is used}
}
\value{
If \eqn{x} is a scalar, the posterior median \eqn{\mbox{med}(\theta|x)}{med(theta|x)} where \eqn{\theta}{theta} 
is the mean of the distribution from which \eqn{x} is drawn.    
If \eqn{x} is a vector with elements \eqn{x_1, ... , x_n}, then the vector returned has elements 
\eqn{\mbox{med}(\theta_i|x_i)}{med(theta_i|x_i)}, where each \eqn{x_i} has mean \eqn{\theta_i}{theta_i}, 
all with the given prior.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{If the quasicauchy prior is used, the argument \code{a} is ignored.
The routine calls the approprate one of \code{postmed.laplace} or \code{postmed.cauchy}.
}
\details{The routine calls the relevant one of the routines \code{postmed.laplace} or \code{postmed.cauchy}.
In the Laplace case, the posterior median is found explicitly, without any need for the numerical solution of an equation.
In the quasi-Cauchy case, the posterior median is found by finding the zero,
component by component, of the vector function \code{cauchy.medzero}.
}
\seealso{
\code{\link{postmean}}}
\examples{
postmed(c(-2,1,0,-4,8,50), w=0.05, prior="cauchy")
postmed(c(-2,1,0,-4,8,50), w=0.2, prior="laplace", a=0.3)
}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
