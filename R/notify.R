notify <- function(message = NULL){
  if (is.null(message)){
    if (Sys.info()[1] == "Windows"){
      winDialog(message = "The R process has finished running.")
    } else {
      msg <- paste('CMD /C "ECHO The R process has finished running. && PAUSE"')
      system(msg, invisible = FALSE, wait = FALSE)
      }
	} else {
	  if (Sys.info()[1] == "Windows"){
	    winDialog(message = message)
	  } else {
	    msg <- paste('CMD /C "ECHO ', message, ' && PAUSE"')
	    system(msg, invisible = FALSE, wait = FALSE)
	  }
	  }}

