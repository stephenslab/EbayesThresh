\name{beta.cauchy}
\alias{beta.cauchy}
\title{Function beta for the quasi-Cauchy prior}
\description{
  Given a value or vector \eqn{x} of values, find the value(s) of the function 
\eqn{\beta(x)=g(x)/\phi(x) - 1}{beta(x) = g(x)/\phi(x) - 1 },
where \eqn{g} is the convolution of the quasi-Cauchy with the 
normal density \eqn{\phi(x)}{\phi(x)}.  
}
\usage{
beta.cauchy(x)
}
\arguments{
  \item{x}{a real value or vector}
}
\value{
  A vector the same length as \eqn{x}, containing the value(s) \eqn{\beta(x)}{beta(x)}.
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{ \code{\link{beta.laplace}}}
\examples{
beta.cauchy(c(-2,1,0,-4,8,50))
}
\keyword{nonparametric}
