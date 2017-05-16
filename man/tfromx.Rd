\name{tfromx}
\alias{tfromx}
\title{Find thresholds from data}
\description{
Given a vector of data and standard deviations (sd equals 1 for Cauchy prior), find the value or vector (heterogeneous sampling standard deviation with Laplace prior) of thresholds corresponding to the marginal maximum likelihood choice of weight.
}
\usage{
tfromx(x, s, prior = "laplace", bayesfac = FALSE, a = 0.5, universalthresh=TRUE)
}
\arguments{
  \item{x}{ vector of data}
  \item{s}{%
	a single value or a vector of standard deviations if the Laplace prior is used. If a vector, must have the same length as \code{x}. Ignored if Cauchy prior is used.
	}
  \item{prior}{specification of prior to be used; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"}}
  \item{bayesfac}{specifies whether Bayes factor threshold should be used instead of posterior median threshold}
  \item{a}{scale factor if Laplace prior is used.  Ignored if Cauchy prior is used.}
  \item{universalthresh}{ if \code{universalthresh=TRUE}, the thresholds will be 
	upper bounded by universal threshold; otherwise, the thresholds can take
	any non-negative values.
	}
}
\details{
First, the routine \code{\link{wfromx}} is called to find the estimated weight. Then the routine \code{\link{tfromw}} is used to find the threshold.
See the documentation for these routines for more details. 
}
\value{
The numerical value or vector of the estimated thresholds is returned.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{tfromw}}, \code{\link{wfromx}}}
\examples{
tfromx(x=rnorm(100, c( rep(0,90), rep(5,10))), prior="cauchy")
}
\keyword{ nonparametric}% at least one, from doc/KEYWORDS
