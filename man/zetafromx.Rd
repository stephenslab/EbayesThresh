\name{zetafromx}
\alias{zetafromx}
\title{Estimation of a parameter in the prior weight sequence in the EbayesThresh paradigm}
\description{
Suppose a sequence of data has underlying mean vector with elements \eqn{\theta_i}.
Given the sequence of data, and a vector of scale factors \code{cs} 
and a lower limit \code{pilo}, 
this routine finds
the marginal maximum likelihood
estimate of the parameter \code{zeta} such that the prior probability of \eqn{\theta_i} being 
nonzero is of the form \code{median(pilo, zeta*cs, 1)}.
}
\usage{
zetafromx(xd, cs, pilo = NA, prior = "laplace", a = 0.5)
}
\arguments{
  \item{xd}{a vector of data}
  \item{cs}{a vector of scale factors, of the same length as \code{x}}
  \item{pilo}{the lower limit for the estimated weights.  If \code{pilo=NA} it 
is calculated according to the sample size to be the weight corresponding to the universal threshold \eqn{\sqrt{2 \log n}}.}
\item{prior}{%
	specification of prior to be used 
	conditional on the mean being nonzero; 
	can be \code{cauchy} or \code{laplace}
	}
  \item{a}{%
	scale factor if Laplace prior is used.  
	Ignored if Cauchy prior is used. 
	If, on entry, \code{a=NA} and \code{prior="laplace"}, then the scale 
	parameter will also be estimated by marginal maximum likelihood. 
	If \code{a} is not specified then the default value 0.5 will be used.
	}
}
\details{
  An exact algorithm is used, based on splitting the range up for \eqn{\zeta} 
into subintervals over which no
element of \eqn{\zeta*\code{cs}} crosses either \code{pilo} or 1.  
Within each of these subintervals, the log likelihood is
concave and its maximum can be found to arbitrary accuracy;  
first the derivatives at each end of the interval are checked
to see if there is an internal maximum at all, and if there is this can be 
found by a binary search for a zero of
the derivative.   
Finally, the maximum of all the local maxima over these subintervals is found.
}
\value{
A list with the following elements
\item{zeta}{The value of \code{zeta} that yields the marginal maximum likelihood}
\item{w}{The weights (prior probabilities of nonzero) yielded by this value of \code{zeta}}
\item{cs}{The factors as supplied to the program}
\item{pilo}{The lower bound on the weight, either as supplied or as calculated internally}
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{ Bernard Silverman}
\note{Once the maximizing \code{zeta} and corresponding weights have been found, 
the thresholds can be found using the program \code{\link{tfromw}}, and
these can be used to process the original data using the routine \code{\link{threshld}}.}
\seealso{\code{\link{tfromw}}, \code{\link{threshld}}, \code{\link{wmonfromx}}, \code{\link{wfromx}}}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
