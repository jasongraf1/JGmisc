cohens.d = function(x, y){
	# calculate cohen's d effect size
	dname <- paste(deparse(substitute(x)), deparse(substitute(y)))
	n1 = length(x)
	n2 = length(y)
	m1 = mean(x)
	m2 = mean(y)
	s1 = sd(x)
	s2 = sd(y)
	ss.var = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
	d = abs(m1-m2)/sqrt(ss.var)
	eff.size <- list(m1 = m1, m2 = m2, ss.variance = ss.var, d = d)
	cat(paste0("\n\tCohen's d measure of effect size\n\n",
						 'd: ', signif(d, 3), '\n',
						 'data: ', dname, '\n',
						 'm1 = ', round(m1, 2), '\tm2 = ', round(m2, 2), '\n',
						 'sd1 = ', round(s1, 2), '\tsd2 = ', round(s2, 2), '\n',
						 'pooled variance: ', round(ss.var, 3),'\n\n')
	)
	return(invisible(eff.size))
}
