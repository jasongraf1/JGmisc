to.char <- function(df, factors.only = T){
	if(factors.only){
		# only convert factors
		id <- sapply(df, is.factor)
		df[id] <- lapply(df[id], as.character)
	} else {
		id <- names(df)
		df[id] <- lapply(df[id], as.character)
	}
	df
}
