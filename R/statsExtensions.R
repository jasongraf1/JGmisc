#############################################################################
# Functions for numerical transformations and other statistics
# v 0.1
#
# Jason Grafmiller
# Aug 04, 2015
#############################################################################


as.numeric2 <- function(x, as.char = T){
	# same as 'as.numeric()' but can be applied to an entire dataframe or matrix
	if(as.char){
		if (is.data.frame(x) || is.matrix(x)) {
			m <- matrix(nrow = nrow(x), ncol = ncol(x))
			colnames(m) <- colnames(x)
			for (i in 1:ncol(x)) {
				if (is.factor(x[,i])){
					m[,i] <- as.numeric(x[,i])
					}
				else m[,i] <- as.numeric(as.character(x[,i]))
			}
			return(as.data.frame(m))
		}
		else return(as.numeric(as.character(x)))
	}
	else {
		if (is.data.frame(x) || is.matrix(x)) {
			m <- matrix(nrow = nrow(x), ncol = ncol(x))
			colnames(m) <- colnames(x)
			for (i in 1:ncol(x)) {
				m[,i] <- as.numeric(x[,i])
			}
			return(as.data.frame(m))
		}
		else return(as.numeric(x))
	}
}


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
	}
	else {
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
	print(mat, quote = F)
}


se <- function (x, na.rm = FALSE) {
	# computing standard error, parallel to sd()
	# 'x' is a vector (or matrix) of numerical values
	if (is.matrix(x)){
		apply(x, 2, sd, na.rm = na.rm)/sqrt(apply(x, 2, length))}
	else if (is.vector(x)){
		x <- as.numeric(x)
		sd(x, na.rm = na.rm)/sqrt(length(x))}
	else if (is.data.frame(x)){
		sapply(x, sd, na.rm = na.rm)/sqrt(sapply(x, length))}
	else sd(as.numeric(x), na.rm = na.rm)/sqrt(length(as.numeric(x)))
}


#========================================================
# Functions for centering and standardizing variables:
#========================================================

c. <- function(x, center = NULL) {
	# function for centering variables
	# x can be vector, matrix or dataframe
	# default is to center around the mean
	if (is.null(center)){
		if (is.data.frame(x) || is.matrix(x)) {
			m <- matrix(nrow = nrow(x), ncol = ncol(x))
			colnames(m) <- paste("c.", colnames(x), sep = "")
			for (i in 1:ncol(x)) {
				m[,i] = x[,i] - mean(x[,i])
			}
			return(as.data.frame(m))
		}
		else {
			x = as.numeric(x)
			return(x - mean(x, na.rm = T))
		}
	}
	else {
		if (is.function(center)){
			if (is.data.frame(x) || is.matrix(x)) {
				m <- matrix(nrow = nrow(x), ncol = ncol(x))
				colnames(m) <- paste("c.", colnames(x), sep = "")
				for (i in 1:ncol(x)) {
					m[,i] <- x[,i] - eval(center(x[,i]))
				}
				return(as.data.frame(m))
			}
			else {
				x = as.numeric(x)
				return(x - eval(center(x)))
			}
		}
		else if (is.character(center)){
			stop("center must be function or numeric")
		}
		else {
			if (is.data.frame(x) || is.matrix(x)) {
			m <- matrix(nrow = nrow(x), ncol = ncol(x))
			colnames(m) <- paste("c.", colnames(x), sep = "")
			for (i in 1:ncol(x)) {
				m[,i] = x[,i] - center
			}
			return(as.data.frame(m))
		}
			else {
				x = as.numeric(x)
				return(x - center)
			}
		}
	}
}


s. <- function (x) {
	## Seber 1977 page 216, from http://dx.doi.org/10.1021/ie970236k
	## Transforms continuous variable to the range [-1, 1]
	## In linked paper, recommended before computing orthogonal
	## polynomials
	(2 * x - max(x) - min(x)) / (max(x) - min(x))
}


z. <- function(x, center = NULL, factor = 1) {
	# general standardizing function
	# default is z-score standardization (center around mean and divide by
	# standard deviation). Can be adjusted to center around specified value
	# and/or multiple SDs by a certain factor, e.g. 2 as recommended by Gelman
	# & Hill (2007:57)
	if (is.null(center)){
		if (is.data.frame(x) || is.matrix(x)) {
			m <- matrix(nrow = nrow(x), ncol = ncol(x))
			colnames(m) <- paste("z.", colnames(x), sep="")
			for (i in 1:ncol(x)) {
				y <- as.numeric(x[,i])
				m[,i] <- (
					(y - mean(y, na.rm = T))/(factor*sd(y, na.rm = T)))
			}
			return(as.data.frame(m))
		}
		else {
			x = as.numeric(x)
			return((x - mean(x, na.rm = T))/(factor*sd(x, na.rm = T)))
		}
	}
	else{
		if (is.numeric(center)){
			if (is.data.frame(x) || is.matrix(x)) {
				m <- matrix(nrow = nrow(x), ncol = ncol(x))
				colnames(m) <- paste("z.", colnames(x), sep = "")
				for (i in 1:ncol(x)) {
					y <- as.numeric(x[,i])
					m[,i] <- (y - center)/(factor*sd(y, na.rm = T))
				}
				return(as.data.frame(m))
			}
			else {
				x = as.numeric(x)
				return((x - center)/(factor*sd(x, na.rm = T)))
			}
		}
		else if (is.function(center)){
			if (is.data.frame(x) || is.matrix(x)) {
				m <- matrix(nrow = nrow(x), ncol = ncol(x))
				colnames(m) <- paste("z.", colnames(x), sep = "")
				for (i in 1:ncol(x)) {
					y <- as.numeric(x[,i])
					m[,i] <- (y - eval(center(y)))/(factor*sd(y, na.rm = T))
				}
				return(as.data.frame(m))
			}
			else {
				x = as.numeric(x)
				return((x - eval(center(x)))/(factor*sd(x, na.rm = T)))
			}
		}
		else stop("center must be numeric or function")
	}
}


#========================================================
# Functions for effect size measures and other tests
#========================================================

cohens.d = function(x, y){
	# calculate cohen's d effect size
	n1 = length(x)
	n2 = length(y)
	m1 = mean(x)
	m2 = mean(y)
	s1 = sd(x)
	s2 = sd(y)
	ss.var = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
	d = abs(m1-m2)/sqrt(ss.var)
	print(paste("Cohen's d: ", signif(d,3)))
	return(list(m1 = m1, m2 = m2, ss.variance = ss.var, d = d))
}


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


# function for g test of independence (likelihood ratio test) to replace chisq
# from -- https://stat.ethz.ch/pipermail/r-help/2001-September/015290.html
g.test <- function(x, y = NULL, correct="none", p = rep(1/length(x), length(x)))
{
	DNAME <- deparse(substitute(x))
	if (is.data.frame(x)) x <- as.matrix(x)
	if (is.matrix(x)) {
		if (min(dim(x)) == 1)
			x <- as.vector(x)
	}
	if (!is.matrix(x) && !is.null(y)) {
		if (length(x) != length(y))
			stop("x and y must have the same length")
		DNAME <- paste(DNAME, "and", deparse(substitute(y)))
		OK <- complete.cases(x, y)
		x <- as.factor(x[OK])
		y <- as.factor(y[OK])
		if ((nlevels(x) < 2) || (nlevels(y) < 2))
			stop("x and y must have at least 2 levels")
		x <- table(x, y)
	}
	if (any(x < 0) || any(is.na(x)))
		stop("all entries of x must be nonnegative and finite")
	if ((n <- sum(x)) == 0)
		stop("at least one entry of x must be positive")
	#If x is matrix, do test of independence
	if (is.matrix(x)) {
		#this block was the separate g.stat function
		cell.tot <- row.tot <- col.tot <- grand.tot <- 0
		nrows<-nrow(x)
		ncols<-ncol(x)
		if (correct=="yates"){ # Do Yates' correction
			if(dim(x)[1]!=2 || dim(x)[2]!=2) # check for 2x2 matrix
				stop("Yates' correction requires a 2 x 2 matrix")
			if((x[1,1]*x[2,2])-(x[1,2]*x[2,1]) > 0)
			{
				x[1,1] <- x[1,1] - 0.5
				x[2,2] <- x[2,2] - 0.5
				x[1,2] <- x[1,2] + 0.5
				x[2,1] <- x[2,1] + 0.5
			}
			else
			{
				x[1,1] <- x[1,1] + 0.5
				x[2,2] <- x[2,2] + 0.5
				x[1,2] <- x[1,2] - 0.5
				x[2,1] <- x[2,1] - 0.5
			}
		}
		# calculate G (Zar, 2000)
		for (i in 1:nrows){
			for (j in 1:ncols){
				if (x[i,j] != 0) cell.tot <- cell.tot + x[i,j] * log10(x[i,j])
			}
		}
		for (i in 1:nrows){ row.tot <- row.tot + (sum(x[i,])) * log10(sum(x[i,])) }
		for (j in 1:ncols){ col.tot <- col.tot + (sum(x[,j])) * log10(sum(x[,j])) }
		grand.tot <- sum(x)*log10(sum(x))
		total <- cell.tot - row.tot - col.tot + grand.tot
		G <- 4.60517 * total
		q <- 1
		if (correct=="williams"){ # Do Williams' correction
			row.tot <- col.tot <- 0
			for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
			for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
			q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
		}
		G <- (G/q)
		# end of old g.stat function

		STATISTIC <- G
		PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
		PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
		if(correct=="none")
			METHOD <- "Log likelihood ratio (G-test) test of independence without correction"
		if(correct=="williams")
			METHOD <- "Log likelihood ratio (G-test) test of independence with Williams' correction"
		if(correct=="yates")
			METHOD <- "Log likelihood ratio (G-test) test of independence with Yates' correction"
	}
	else {
		# x is not a matrix, so we do Goodness of Fit
		METHOD <- "Log likelihood ratio (G-test) goodness of fit test"
		if (length(x) == 1)
			stop("x must at least have 2 elements")
		if (length(x) != length(p))
			stop("x and p must have the same number of elements")
		E <- n * p

		if (correct=="yates"){ # Do Yates' correction
			if(length(x)!=2)
				stop("Yates' correction requires 2 data values")
			if ( (x[1]-E[1]) > 0.25) {
				x[1] <- x[1]-0.5
				x[2] <- x[2]+0.5
			}
			else if ( (E[1]-x[1]) > 0.25){
				x[1] <- x[1]+0.5
				x[2] <- x[2]-0.5
			}
		}
		names(E) <- names(x)
		tot <- 0
		for (i in 1:length(x)){
			if (x[i] != 0) tot <- tot + x[i] * log(x[i]/E[i])
		}
		G <- (2*tot)
		if (correct=="williams"){ # Do Williams' correction
			q <- 1+(length(x)+1)/(6*n)
			G <- (G/q)
		}
		STATISTIC <- (G)
		PARAMETER <- length(x) - 1
		PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
	}
	names(STATISTIC) <- "Log likelihood ratio statistic (G)"
	names(PARAMETER) <- "X-squared df"
	names(PVAL) <- "p.vlaue"
	structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
								 method=METHOD,data.name=DNAME),class="htest")
}

#############################################################################
