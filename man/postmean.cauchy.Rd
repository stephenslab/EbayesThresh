\name{postmean.cauchy}
\alias{postmean.cauchy}
\title{Posterior mean estimator using quasi-Cauchy prior}
\description{
Given a data value or a vector of data, find the posterior mean estimate(s) 
of the underlying signal value(s). 
The quasi-Cauchy distribution is used for the nonzero component of the prior
}
\usage{
postmean.cauchy(x, w)
}
\arguments{
  \item{x}{data value or vector of data}
  \item{w}{prior probability that signal is nonzero}
}
\value{
A value or vector of values of the estimate(s) of the mean(s) of the distribution(s) from which the 
\eqn{x} are drawn.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{The posterior mean is found by expressing the quasi-Cauchy prior as a mixture. 
The mean conditional on the mixing parameter is found and is then averaged 
over the posterior distribution of the mixing parameter, 
including the atom of probability at zero variance.
}
\seealso{
\code{\link{postmean}},
\code{\link{postmean.laplace}},
\code{\link{postmed.cauchy}}
}
\examples{
postmean.cauchy(c(-2,1,0,-4,8,50), w=0.05)
}
\keyword{nonparametric}
