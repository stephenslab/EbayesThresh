\name{postmed.cauchy}
\alias{postmed.cauchy}
\title{Posterior median estimator using quasi-Cauchy prior}
\description{
Given a data value or a vector of data, find the posterior median estimate(s) 
of the underlying signal value(s). 
The quasi-Cauchy distribution is used for the nonzero component of the prior
}
\usage{
postmed.cauchy(x, w)
}
\arguments{
  \item{x}{data value or vector of data}
  \item{w}{prior probability that signal is nonzero}
}
\value{
A value or vector of values of the estimate(s) of the mean(s) of the distribution(s) from which the \eqn{x} are drawn.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{The posterior median is found by finding the zero of the function \code{\link{cauchy.medzero}}.
}
\seealso{
\code{\link{postmed}},
\code{\link{postmed.laplace}},
\code{\link{postmean.cauchy}},
\code{\link{cauchy.medzero}}
}
\examples{
postmed.cauchy (c(-2,1,0,-4,8,50), w=0.05)
}
\keyword{nonparametric} 
