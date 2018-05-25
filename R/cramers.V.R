cramers.V <- function(tab){
	# calculate cramer's V effect size
	n = sum(tab)
	chisq = chisq.test(tab)$statistic
	phisq = chisq/n
	c = ncol(tab)
	r = nrow(tab)
	V = sqrt(phisq/min(r - 1, c - 1))
	s = c(phisq, V)
	names(s) <- c("Phi coefficient","Cramer's V")
	return(s)
}
