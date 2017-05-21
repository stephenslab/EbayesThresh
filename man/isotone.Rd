\name{isotone}
\alias{isotone}
\title{Weighted least squares monotone regression}
\description{Given a vector of data and a vector of weights, find the
monotone sequence closest to the data in the sense of weighted least
squares with the given weights.}
\usage{
isotone(x, wt = rep(1, length(x)), increasing = FALSE)
}
\arguments{
  \item{x}{a vector of data}
  
  \item{wt}{a vector the same length as \code{x}, giving the weights to
    be used in the weighted least squares algorithm}
  
  \item{increasing}{logical variable indicating whether the required fit
    is to be increasing or decreasing}
}
\details{
The standard pool-adjacent-violators algorithm is used. Maximal
decreasing subsequences are found within the current sequence. Each
such decreasing subsequence is replaced by a constant sequence with
value equal to the weighted average. Within the algorithm, the
subsequence is replaced by a single point, with weight the sum of the
weights within the subsequence. This process is iterated to termination.
The resulting sequence is then unpacked back to the original ordering to
give the weighted least squares monotone fit.

If \code{increasing = FALSE}, the original sequence is negated and the
resulting estimate negated.
}
\value{The vector giving the best fitting monotone sequence is
  returned.}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wmonfromx}}}
\keyword{internal}
