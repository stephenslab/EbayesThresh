\name{cauchy.threshzero}
\alias{cauchy.threshzero}
\title{Objective function for quasi-Cauchy threshold}
\description{
The objective function that has to be zeroed
to find the posterior median threshold when the quasi-Cauchy prior is used
}
\usage{
cauchy.threshzero(z, w)
}
\arguments{
  \item{z}{vector of putative threshold values}
  \item{w}{vector of prior probabilities that parameter is nonzero (can be scalar)}
}
\details{
The function is called by the routine \code{\link{tfromw}} 
which finds the threshold for particular weight(s) \code{w}.
}
\value{
Vector of objective function values
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{ 
\code{\link{laplace.threshzero}},
\code{\link{tfromw}}}
\keyword{internal}% at least one, from doc/KEYWORDS
