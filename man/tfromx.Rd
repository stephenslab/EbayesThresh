\name{tfromx}
\alias{tfromx}
\title{Find threshold from data}
\description{
Given a vector of data, find the threshold corresponding to the marginal maximum likelihood choice of weight.
}
\usage{
tfromx(x, prior = "laplace", bayesfac = FALSE, a = 0.5)
}
\arguments{
  \item{x}{ vector of data}
  \item{prior}{specification of prior to be used; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"}}
\item{bayesfac}{specifies whether Bayes factor threshold should be used instead of posterior median threshold}
\item{a}{scale factor if Laplace prior is used.  Ignored if Cauchy prior is used.}
}
\details{
First, the routine \code{\link{wfromx}} is called to find the estimated weight. Then the routine \code{\link{tfromw}} is used to find the threshold.
See the documentation for these routines for more details. 
}
\value{
The numerical value of the estimated threshold is returned.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{tfromw}}, \code{\link{wfromx}}}
\examples{
tfromx(x=rnorm(100, c( rep(0,90), rep(5,10))), prior="cauchy")
}
\keyword{ nonparametric}% at least one, from doc/KEYWORDS
