\name{wandafromx}
\alias{wandafromx}
\title{Find weight and scale factor from data if Laplace prior is used}
\description{
Given a vector of data, find the marginal maximum likelihood choice of both weight and scale factor under the Laplace prior.
}
\usage{
wandafromx(x)
}
\arguments{
  \item{x}{a vector of data}
}
\details{
The parameters are found by marginal maximum likelihood. 
The search is over weights corresponding to thresholds in the range \eqn{[0, \sqrt{2 \log n}]}{[0, sqrt(2 log n)]},
where \eqn{n} is the length of the data vector.
The search uses a nonlinear optimization routine (\code{\link{optim}} in \R)
to minimize the negative log likelihood function \code{\link{negloglik.laplace}}. 
The range over which the scale factor is searched is (0.04, 3).
}
\value{
A list with values
  \item{w}{The estimated weight}
  \item{a}{The estimated scale factor}
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wfromx}}, \code{\link{tfromw}}}
\examples{
wandafromx(rnorm(100, c( rep(0,90), rep(5,10))))
}
\keyword{nonparametric}
