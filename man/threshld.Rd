\name{threshld}
\alias{threshld}
\title{Threshold data with hard or soft thresholding}
\description{
  Given a data value or a vector of data, threshold the data at a specified value, using hard or soft thresholding
}
\usage{
threshld(x, t, hard = TRUE)
}
\arguments{
  \item{x}{ a data value or a vector of data }
  \item{t}{value of threshold to be used}
  \item{hard}{ specifies whether hard or soft thresholding is applied}
}
\value{
  A value or vector of values the same length as \code{x}, containing the result of the relevant thresholding rule applied to \code{x}. 

}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{ \code{\link{ebayesthresh}}}
\examples{
threshld(-5:5, 1.4, FALSE)
}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
