notify <- function(x = NULL){
  if (is.null(x)){
    msg <- 'CMD /C "ECHO The R process has finished running && PAUSE"'
    if (Sys.info()[1] == "Windows"){
      winDialog(message = msg)
    } else {
      system(msg, invisible = FALSE, wait = FALSE)
      }
	} else {
	  msg <- paste('CMD /C "ECHO ', x, ' && PAUSE"')
	  if (Sys.info()[1] == "Windows"){
	    windDialog(message = msg)
	  } else {
	    system(msg, invisible = FALSE, wait = FALSE)
	  }
	  }}

