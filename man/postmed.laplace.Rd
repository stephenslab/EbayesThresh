\name{postmed.laplace}
\alias{postmed.laplace}
\title{Posterior median estimator using Laplace prior}
\description{Find posterior median estimator given data, using the Laplace prior for the nonzero component}
\usage{
postmed.laplace(x, w, a = 0.5)
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
\note{The posterior median is found explicitly, without any need for the numerical solution of an equation.
}
\seealso{
\code{\link{postmed}},
\code{\link{postmed.cauchy}},
\code{\link{postmean.laplace}}
}
\examples{
postmed.laplace (c(-2,1,0,-4,8,50), w=0.05, a=0.3)
}
\keyword{nonparametric}
