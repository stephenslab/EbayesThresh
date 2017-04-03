"beta.laplace" <-
function(x, s, a = 0.5)
{
#
#  The function beta for the Laplace prior with parameter a
#
	x <- abs(x)
	xpa <- x/s + s*a
	xma <- x/s - s*a
	rat1 <- 1/xpa
	rat1[xpa < 35] <- pnorm( - xpa[xpa < 35])/dnorm(xpa[xpa < 35])
	xma[abs(xma) > 35] <- sign(xma[abs(xma) > 35]) * 35
	rat2 <- pnorm(xma)/dnorm(xma)
	beta <- (a/2) * (rat1 + rat2) - 1
	return(beta)
}