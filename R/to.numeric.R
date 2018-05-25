to.numeric <- function(df, id = NULL){
	if(is.null(id)){
		id <- names(df)
		df[id] <- lapply(df[id],
			function(x) as.numeric(as.character(x)))
		} else {
		df[id] <- lapply(df[id],
			function(x) as.numeric(as.character(x)))
		}
	df
}
