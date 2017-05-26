beta.laplace <- function(x, s = 1, a = 0.5) {
#
#  The function beta for the Laplace prior given parameter a and s (sd)
#
	x <- abs(x)
	xpa <- x/s + s*a
	xma <- x/s - s*a
	rat1 <- 1/xpa
	rat1[xpa < 35] <- pnorm( - xpa[xpa < 35])/dnorm(xpa[xpa < 35])
	rat2 <- 1/abs(xma)
	xma[xma > 35] <- 35
	rat2[xma > -35] <- pnorm(xma[xma > -35])/dnorm(xma[xma > -35])
	beta <- (a * s) / 2 * (rat1 + rat2) - 1
	return(beta)
}
