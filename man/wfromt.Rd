\name{wfromt}
\alias{wfromt}
\title{Mixing weight from posterior median threshold}
\description{
Given a value or vector of thresholds and sampling standard deviations (sd equals 1 for Cauchy prior), find the mixing weight for which this is(these are) the threshold(s) of the posterior median estimator.
If a vector of threshold values is provided, the vector of corresponding weights is returned.
}
\usage{
wfromt(tt, s, prior = "laplace", a = 0.5)
}
\arguments{
  \item{tt}{ threshold value or vector of values}
  \item{s}{ a single value or a vector of standard deviations if the Laplace prior is used. If a vector, must have the same length as \code{tt}. Ignored if Cauchy prior is used.}
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

\examples{
wfromt( c(2,3,5), prior="cauchy" )
}
\keyword{ nonparametric}% at least one, from doc/KEYWORDS
