#' Save list of installed packages
#'
#' @param file Name of the file to save to. Must be a full path specification. If \code{NULL}, it will save to the current working directory.
#'
#' @details Saves a workspace file (\code{.Rdata}) containing the complete list of all packages currently installed.
#' @return Nothing
#' @export
#'
#' @examples
save.pkg.list <- function(file = NULL){
	# create a temporary environment
	tmp.env <- new.env()
	# Save list of packages into temporary environment
	tmp.env$pkgs <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]

	if(is.null(file)) {
	  wd <- getwd()
	  filename <- paste0("pkg_list_", format(Sys.time(), "%Y-%m-%d"), ".RData")
	  file <- paste(wd, filename, sep = "/")
	  }
	save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, file = file)
	cat("Saving package list to:\n", file)
	rm(tmp.env)
}
