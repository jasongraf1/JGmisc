to.factor <- function(df, id = NULL){
	if(is.null(id)){
		id <- names(df)
		df[id] <- lapply(df[id], as.factor)
	} else {
		df[id] <- lapply(df[id], as.factor)
	}
	df
}

