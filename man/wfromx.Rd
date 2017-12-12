\name{wfromx}
\alias{wfromx}
\title{Find Empirical Bayes weight from data}
\description{
Suppose the vector \eqn{(x_1, \ldots, x_n)} is such that \eqn{x_i} is
drawn independently from a normal distribution with mean
\eqn{\theta_i}{\theta_i} and standard deviation \eqn{s_i} (s_i equals 1
for Cauchy prior). The prior distribution of the
\eqn{\theta_i}{theta_i} is a mixture with probability \eqn{1-w} of zero
and probability \eqn{w} of a given symmetric heavy-tailed distribution.
This routine finds the marginal maximum likelihood estimate of the
parameter \eqn{w}.
}
\usage{
wfromx(x, s = 1, prior = "laplace", a = 0.5, universalthresh = TRUE)
}
\arguments{
  \item{x}{Vector of data.}
  
  \item{s}{A single value or a vector of standard deviations if the
  Laplace prior is used. If a vector, must have the same length as
  \code{x}. Ignored if Cauchy prior is used.}
      
  \item{prior}{Specification of prior to be used; can be
    \code{"cauchy"} or \code{"laplace"}.}
  
  \item{a}{Inverse scale (\emph{i.e.}, rate) parameter if Laplace prior
    is used. Ignored if Cauchy prior is used.}
  
  \item{universalthresh}{If \code{universalthresh = TRUE}, the
	thresholds will be upper bounded by universal threshold;
	otherwise, the thresholds can take any non-negative values.}
}
\details{
The weight is found by marginal maximum likelihood.
  
The search is over weights corresponding to threshold \eqn{t_i} in the
range \eqn{[0, s_i \sqrt{2 \log n}]}{[0, s_i sqrt(2 log n)]} if
\code{universalthresh=TRUE}, where \eqn{n} is the length of the data
vector and \eqn{(s_1, ... , s_n)} (s_i is 1 for Cauchy prior) is the
vector of sampling standard deviation of data \eqn{(x_1, ... , x_n)};
otherwise, the search is over \eqn{[0, 1]}.

The search is by binary search for a solution to the equation 
\eqn{S(w)=0}, where \eqn{S} is the derivative of the log likelihood.
The binary search is on a logarithmic scale in \eqn{w}.

If the Laplace prior is used, the inverse scale parameter is fixed at
the value given for \code{a}, and defaults to 0.5 if no value is
provided. To estimate \code{a} as well as \code{w} by marginal maximum
likelihood, use the routine \code{\link{wandafromx}}.
}
\value{The numerical value of the estimated weight.}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wandafromx}}, \code{\link{tfromx}},
  \code{\link{tfromw}}, \code{\link{wfromt}}
}
\examples{
wfromx(x = rnorm(100, s = c(rep(0,90),rep(5,10))), prior = "cauchy")
}
\keyword{nonparametric}
