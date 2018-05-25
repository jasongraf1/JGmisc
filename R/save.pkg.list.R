save.pkg.list <- function(file = NULL){
	# create a temporary environment
	tmp.env <- new.env()
	# Save list of packages into temporary environment
	tmp.env$pkgs <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]

	if(is.null(file)) file <- "~/pkglist.Rdata"
	save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, file = file)
	rm(tmp.env)
}
