\name{wmonfromx}
\alias{wmonfromx}
\title{ 
Find monotone Empirical Bayes weights from data 
}
\description{
Given a vector of data, find the marginal maximum likelihood choice of 
weight sequence subject to the constraints that the 
weights are monotone decreasing.
}
\usage{
wmonfromx(xd, prior = "laplace", a = 0.5, tol = 1e-08, maxits = 20)
}
\arguments{
  \item{xd}{a vector of data}
  \item{prior}{specification of the prior to be used; can be \code{cauchy} or 
	\code{laplace}}
  \item{a}{scale parameter in prior if \code{prior="laplace"}.  Ignored if \code{prior="cauchy"}}
  \item{tol}{absolute tolerance to within which estimates are calculated}
  \item{maxits}{maximum number of weighted least squares iterations within the calculation}
}
\details{
The weights is found by marginal maximum likelihood. 
The search is over weights corresponding to thresholds in the range 
\eqn{[0, \sqrt{2 \log n}]}, where \eqn{n} is the length of the data vector.

An iterated least squares monotone regression algorithm 
is used to maximize the log likelihood. 
The weighted least squares monotone regression routine 
\code{\link{isotone}} is used.

To turn the weights into thresholds, use the routine \code{\link{tfromw}}; 
to process the data with these thresholds, 
use the routine \code{\link{threshld}}.
}
\value{
The vector of estimated weights is returned
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{ \code{\link{wfromx}}, \code{\link{isotone}}}
\examples{}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
