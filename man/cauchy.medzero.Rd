\name{cauchy.medzero}
\alias{cauchy.medzero}
\title{Objective function for quasi-Cauchy posterior median}
\description{
The objective function that has to be zeroed, component by component, 
to find the 
posterior median when the quasi-Cauchy prior is used. 
}
\usage{
cauchy.medzero(x, z, w)
}
\arguments{
  \item{x}{vector of parameter values (may be scalar)}
  \item{z}{data vector or value}
  \item{w}{prior probability that parameter is nonzero}
}
\value{
vector of values of objective function
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{postmed.cauchy}}}
\keyword{internal}
