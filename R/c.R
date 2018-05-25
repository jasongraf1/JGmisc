c. <- function(x, center = NULL) {
	y <- as.numeric(x)
	if(is.null(center)) return (y - mean(y))
	else return (y - center)
}
