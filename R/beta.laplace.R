"beta.laplace" <-
function(x, a = 0.5)
{
#
#  The function beta for the Laplace prior with parameter a
#
	x <- abs(x)
	a <- min(a, 35)
	xpa <- x + a
	xma <- x - a
	rat1 <- 1/xpa
	rat1[xpa < 35] <- pnorm( - xpa[xpa < 35])/dnorm(xpa[xpa < 35])
	rat2 <- 1/abs(xma)
	xma[xma > 35] <- 35
	rat2[xma > -35] <- pnorm(xma[xma > -35])/dnorm(xma[xma > -35])
	beta <- (a * s) / 2 * (rat1 + rat2) - 1
	return(beta)
}
