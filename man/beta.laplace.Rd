\name{beta.laplace}
\alias{beta.laplace}
\title{Function beta for the Laplace prior}
\description{
Given a single value or a vector of \eqn{x} and \eqn{s}, find the
value(s) of the function \eqn{\beta(x;s,a)=g(x;s,a)/fn(x;0,s) -
1}{beta(x;s,a) = g(x;s,a)/fn(x;0,s) - 1 }, where \eqn{fn(x;0,s)} is the
normal density with mean 0 and standard deviation \eqn{s}, and \eqn{g}
is the convolution of the Laplace density with scale parameter \eqn{a},
\eqn{\gamma_a(\mu)}{gamma(mu; a)}, with the normal density
\eqn{fn(x;\mu,s)}{fn(x;mu,s)} with mean \eqn{mu} and standard deviation
\eqn{s}.
}
\usage{beta.laplace(x, s = 1, a = 0.5)}
\arguments{
  \item{x}{the value or vector of data values}
  
  \item{s}{the value or vector of standard deviations; if vector, must
    have the same length as \code{x}}
  
  \item{a}{the scale parameter of the Laplace distribution}
}
\value{
  A vector of the same length as \code{x} is returned,
  containing the value(s) \eqn{beta(x)}.
}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{
The Laplace density is given by \eqn{\gamma(u;a) = \frac{1}{2} a
e^{-a|u|}}{gamma(u) = (a/2) exp(-a|u|)} and is also known as the
double exponential density.
}
\seealso{\code{\link{beta.cauchy}}}
\examples{
beta.laplace(c(-2,1,0,-4,8,50), s=1)
beta.laplace(c(-2,1,0,-4,8,50), s=1:6, a=1)
}
\keyword{nonparametric}
