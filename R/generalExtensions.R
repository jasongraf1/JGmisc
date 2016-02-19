#############################################################################
#
# Some general useful functions for working in R
# v 0.1
#
# Jason Grafmiller
# Aug 04, 2015
#############################################################################

detachAllPackages <- function(keep = NULL, keep.basic = TRUE) {
	# function for detaching all attached packages (except basic ones)
	basic.packages <- c("package:stats","package:graphics","package:grDevices",
											"package:utils","package:datasets","package:methods",
											"package:base")
	package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
																	TRUE, FALSE)]
	if (!is.null(keep)){
		package.list <- setdiff(package.list, paste("package", keep, sep = ":"))
	}
	if (keep.basic){
		package.list <- setdiff(package.list, basic.packages)
	}
	if (length(package.list) > 0) {
		for (package in package.list) detach(package, character.only = TRUE)
	}
}


filter.infrequent <- function(words, threshold = 5, dummy = "OTHER") {
	# code from WBRS for recoding infrequent factor levels (default is <= 5
	# observations)
	return (relevel(
		as.factor(
			ifelse(words %in% levels(as.factor(words))[table(words) >= threshold],
						 as.character(words), dummy)), dummy))
}


insert <- function(v, e, pos){
	# inserts item 'e' into vector 'v' at position 'pos'
	return(c(v[1:(pos-1)], e, v[(pos):length(v)]))
}


## variables and functions for adding captions to tables, and figures in html
# Captions from Benedikt Heller. The following variables must be defined:
# n_tables   <<- 0;
# n_figures  <<- 0;
# n_listings <<- 0;

insert_caption <- function(type, text) {
	html <- "<p class='caption'><span class='graphtype'>"
	if (type=="table") {
		n_tables <<- n_tables + 1;
		html <- paste(html, "Table ", n_tables, "</span>.", sep="");
	}
	else if (type=="figure") {
		n_figures <<- n_figures + 1;
		html <- paste(html, "Figure ", n_figures, "</span>.", sep="");
	}
	else if (type=="listing") {
		n_listings <<- n_listings + 1;
		html <- paste(html, "Listing ", n_listings, "</span>.", sep="");
	}
	html <- paste(html, text, "</p>");
	I(html)
}


interleave.v <- function(x, y){
	# function for interleaving two vectors
	# different from the gdata package's interleave() function
	# Example;
	# > interleave(1:3, letters[1:3])
	# > [1] "1" "a" "2" "b" "c"
	m0 <- length(x); n0 <- length(y)
	m <- min(m0, n0); n <- max(m0, n0)
	z <- numeric(m + n)
	if(m0==n0){
		z[(-1+2*(1:m))] = x
		z[2*(1:m)] = y
	}
	else {
		if(m0 < n0){
			z[(-1+2*(1:m))] = x
			z[2*(1:m)] = y[1:m]
			z[(2*m+1):(m+n)] = y[(m+1):n]
		} else {
			z[(2*(1:m))] = y
			z[-1+2*(1:m)] = x[1:m]
			z[(2*m+1):(m+n)] = x[(m+1):n]
		}
	}
	return(z)
}


join2 <- function (strings, sep = "", finalSep = NULL){
	# joins elements in a vector into a single string
	# different from the plyr package's join() function
	strings <- as.character(strings)
	if (length(strings) == 1)
		return(strings[1])
	else if (length(strings) > 1) {
		result <- strings[1]
		for (i in 2:length(strings)) {
			if (i == length(strings) && !is.null(finalSep))
				result <- paste(result, strings[i], sep = finalSep)
			else result <- paste(result, strings[i], sep = sep)
		}
		return(result)
	}
}


my.library <- function(packages){
	invisible(sapply(packages, silent.load))
}


notify <- function(x = NULL){
	if (is.null(x)){
		system('CMD /C "ECHO The R process has finished running && PAUSE"',
					 invisible=FALSE, wait=FALSE)
	}
	else { system(paste('CMD /C "ECHO ', x, ' && PAUSE"'),
					invisible=FALSE, wait=FALSE) }
}

rm.func <- function (){
	rm(list = ls()[sapply(ls(), function(n){is.function(get(n))})])
}


rm.nonfunc <- function (){
	rm(list = ls()[sapply(ls(), function(n){!is.function(get(n))})])
}


silent.load <- function(a.package){
	suppressWarnings(suppressPackageStartupMessages(
		library(a.package, character.only = TRUE)))
}

#############################################################################
