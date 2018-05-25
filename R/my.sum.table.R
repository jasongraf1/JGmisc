my.sum.table <- function(tab, d = 2, rnd = 1) { # needs work...
	# function for creating a table with counts and percentages
	# requires the interleave.v() function
	# 'd' is the dimension argument for prop.table to calculate proportions. The
	# default is by columns (2)
	# 'rnd' is the number of digits for rounding the percentages
	if (length(dim(tab)) == 1){
		props <- round(prop.table(tab) * 100, rnd)
		mat <- interleave.v(tab, props)
		names(mat) <- interleave.v(names(tab), rep("%", length(tab)))
	} else {
		margins <- addmargins(tab)
		props <- round(prop.table(tab, d) * 100, rnd)
		mat <- matrix(nrow = nrow(margins), ncol = 1 + ncol(tab) * 2, byrow = T)
		rownames(mat) <- c(rownames(tab), "Total")
		colnames(mat) <- c(interleave.v(colnames(tab),
																		rep("%", ncol(tab))), "Total")
		for(i in seq(1, nrow(tab))){
			mat[i,] = c(interleave.v(tab[i,], round(props[i,], rnd)), sum(tab[i,]))
		}
		rtotal = interleave.v(margins[nrow(margins),], rep("", ncol(tab)))
		mat[nrow(mat),] = rtotal
	}
	return (mat)
}
