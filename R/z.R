z. <- function(x, center = NULL, factor = 1) {
	x <- as.numeric(as.character(x))
	if (is.null(center)) {
		return (x - mean(x))/(sd(x * factor))
	} else return (x - center)/(sd(x * factor))
	}

