\name{negloglik.laplace}
\alias{negloglik.laplace}
\title{Negative log likelihood of parameters of mixed Laplace prior}
\description{
This routine calculates the negative log likelihood of the two parameters of the mixed Laplace prior.
It is intended as an auxiliary routine to be called by the 
optimization routine within \code{\link{wandafromx}}.
For reasons of numerical stability within the optimization, the prior is parametrized by the 
threshold and the scale parameter.  The routines \code{\link{tfromw}} and \code{\link{wfromt}}
establish the mapping between the threshold and the mixing weight, for any fixed value of the scale parameter.
}
\usage{
negloglik.laplace(xpar, xx)
}
\arguments{
  \item{xpar}{A vector of two parameters: 
	\code{xpar[1]} is the threshold and \code{xpar[2]} is the scale factor \code{a}.}
  \item{xx}{vector of data}
}
\value{
The negative log likelihood is returned
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{beta.laplace}},\code{\link{wandafromx}}}
\keyword{internal}% at least one, from doc/KEYWORDS
