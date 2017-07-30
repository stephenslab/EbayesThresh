\name{wandafromx}
\alias{wandafromx}
\alias{negloglik.laplace}
\title{Find weight and scale factor from data if Laplace prior is used.}
\description{
Given a vector of data and a single value or vector of sampling standard
deviations, find the marginal maximum likelihood choice of both weight
and scale factor under the Laplace prior.
}
\usage{
wandafromx(x, s = 1, universalthresh = TRUE)
negloglik.laplace(xpar, xx, ss, tlo, thi)
}
\arguments{
  \item{x}{A vector of data.}
  
  \item{s}{A single value or a vector of standard deviations. If
    vector, must have the same length as \code{x}.}
  
  \item{universalthresh}{If \code{universalthresh = TRUE}, the thresholds
	will be upper bounded by universal threshold; otherwise, the
	thresholds can take any non-negative values.}

  \item{xx}{A vector of data.}
      
  \item{xpar}{Vector of two parameters: \code{xpar[1]} : a value between
      0 and 1, which will be adjusted to range of w; \code{xpar[2]}, 
      scale factor "a".}

  \item{ss}{Vector of standard deviations.}

  \item{tlo}{Lower bound of thresholds.}

  \item{thi}{Upper bound of thresholds.}
}
\details{
The parameters are found by marginal maximum likelihood.
  
The search is over weights corresponding to threshold \eqn{t_i} in the
range \eqn{[0, s_i \sqrt{2 \log n}]}{[0, s_i sqrt(2 log n)]} if
\code{universalthresh=TRUE}, where \eqn{n} is the length of the data
vector and \eqn{(s_1, ... , s_n)} is the vector of sampling standard
deviation of data \eqn{(x_1, ... , x_n)}; otherwise, the search is over
\eqn{[0,1]}.

The search uses a nonlinear optimization routine (\code{\link{optim}} in
\R) to minimize the negative log likelihood function
\code{negloglik.laplace}.  The range over which the scale factor is
searched is (0.04, 3).  For reasons of numerical stability within the
optimization, the prior is parametrized internally by the threshold and
the scale parameter.
}
\value{
A list with values:

\item{w}{The estimated weight.}

\item{a}{The estimated scale factor.}
}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wfromx}}, \code{\link{tfromw}}}
\examples{
wandafromx(rnorm(100, c(rep(0,90),rep(5,10))), s = 1)
}
\keyword{nonparametric}
