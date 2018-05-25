load.cache <- function(path = "./cache", envir = parent.frame(), ask = FALSE,
	verbose = TRUE, full.names = TRUE, ...) {
	#' Taken from http://www.peteredewitt.com/2017/lazyloading-cached-chunks/
	files <- do.call(list.files, list(path = path, pattern = "\\.rdx$", full.names = full.names, ...))
	files <- gsub("\\.rdx", "", files)
	load_these <- rep(TRUE, length(files))
	if (ask) {
		for (i in seq_along(files)) {
			answer <- readline(prompt = paste("load database:", gsub("_[0-9a-f]{32}", "", files[i]), "(y/n)"))
			load_these[i] <- answer %in% c("Yes", "yes", "Y", "y")
		}}
	files <- files[load_these]
	if (!verbose) {
		sapply(files, lazyLoad, envir = envir)
	} else {
		sapply(files,
					 function(x, envir) {
					 	message(paste("Lazyloading:", gsub("_[0-9a-f]{32}", "", x)))
					 	lazyLoad(x, envir = envir) },
					 envir = envir)
	}
	invisible()
}
