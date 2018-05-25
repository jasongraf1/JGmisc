to.char <- function(df, factors.only = TRUE){
	if(factors.only){
		id <- sapply(df, is.factor)
		df[id] <- lapply(df[id], as.factor)
	} else {
		id <- names(df)
		df[id] <- lapply(df[id], as.factor)
	}
	df
}
