notify <- function(x = NULL){
	if (is.null(x)){
		system('CMD /C "ECHO The R process has finished running && PAUSE"',
					 invisible=FALSE, wait=FALSE)
	} else {
		system(paste('CMD /C "ECHO ', x, ' && PAUSE"'),
					 invisible=FALSE, wait=FALSE) }
}
