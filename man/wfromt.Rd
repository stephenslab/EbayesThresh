\name{wfromt}
\alias{wfromt}
\title{Mixing weight from posterior median threshold}
\description{
Given a threshold value, find the mixing weight for which this is the threshold of the posterior median estimator.
If a vector of threshold values is provided, the vector of corresponding weights is returned.
}
\usage{
wfromt(tt, prior = "laplace", a = 0.5)
}
\arguments{
  \item{tt}{ threshold value or vector of values}
  \item{prior}{specification of prior to be used; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"}}
  \item{a}{scale factor if Laplace prior is used.  Ignored if Cauchy prior is used. }
}
\value{
  The numerical value or vector of values of the corresponding weight is returned.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{ \code{\link{tfromw}}}
}
\examples{
wfromt( c(2,3,5), prior="cauchy" )
}
\keyword{ nonparametric}% at least one, from doc/KEYWORDS
