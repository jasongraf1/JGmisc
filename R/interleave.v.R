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
	} else {
		if(m0 < n0){
			z[(-1+2*(1:m))] = x
			z[2*(1:m)] = y[1:m]
			z[(2*m+1):(m+n)] = y[(m+1):n]
		} else {
			z[(2*(1:m))] = y
			z[-1+2*(1:m)] = x[1:m]
			z[(2*m+1):(m+n)] = x[(m+1):n]
		}}
	return(z)
}
