\name{beta.laplace}
\alias{beta.laplace}
\title{Function beta for the Laplace prior}
\description{
  Given a value or vector \eqn{x} of values, find the value(s) of the function 
\eqn{\beta(x)=g(x)/\phi(x) - 1}{beta(x) = g(x)/phi(x) - 1 },
where \eqn{g} is the convolution of the Laplace density with scale parameter \eqn{a} with  with the normal density \eqn{\phi(x)}{phi(x)}.  
}
\usage{
beta.laplace(x, a = 0.5)
}
\arguments{
  \item{x}{the value or vector of data values}
  \item{a}{the scale parameter of the Laplace distribution}
}
\value{
  A vector the same length as \eqn{\code{x}}{x} is returned, containing the value(s) \eqn{\beta(\code{x})}{beta(x)}.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{ 
The Laplace density is given by 
\eqn{\gamma(u) = \frac{1}{2} a e^{-a|u|}}{gamma(u)= (a/2) exp ( - a|u| ) } 
and is also known as the double exponential density.}
\seealso{ \code{\link{beta.cauchy}}}
\examples{
beta.laplace(c(-2,1,0,-4,8,50))
beta.laplace(c(-2,1,0,-4,8,50), a=1)
}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
