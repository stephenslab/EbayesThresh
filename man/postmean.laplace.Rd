\name{postmean.laplace}
\alias{postmean.laplace}
\title{Posterior mean estimator using Laplace prior}
\description{Find posterior mean estimator given data, when the Laplace prior is used for the nonzero component 
of the signal}
\usage{
postmean.laplace(x, w, a = 0.5)
}
\arguments{
  \item{x}{ data value or vector of data}
  \item{w}{ prior probability that signal is nonzero}
  \item{a}{ scale parameter of Laplace part of the prior}
}
\value{
A value or vector of values of the estimate(s) of the mean(s) of the distribution(s) from which the \eqn{x} are drawn.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{
The posterior mean is found explicitly. 
The posterior probability that the parameter is nonzero is found, 
as is the posterior mean conditional on not being zero; 
the product of these is the posterior mean.
}
\seealso{
\code{\link{postmean}},
\code{\link{postmean.cauchy}},
\code{\link{postmed.laplace}}
}
\examples{
postmean.laplace(c(-2,1,0,-4,8,50), w=0.05, a=0.3)
}
\keyword{nonparametric}
