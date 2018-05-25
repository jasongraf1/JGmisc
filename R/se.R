se <- function (x, na.rm = FALSE) {
	to.num <- function(x) as.numeric(as.character(x))
	if (is.matrix(x)){
		apply(x, 2, FUN = function(v) sd(to.num(v), na.rm = na.rm))/sqrt(apply(x, 2, length))}
	else if (is.data.frame(x)){
		sapply(x, FUN = function(v) sd(to.num(v), na.rm = na.rm))/sqrt(sapply(x, length))}
	else sd(to.num(x), na.rm = na.rm)/sqrt(length(x))
}
