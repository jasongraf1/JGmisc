filter.infrequent <- function(words, threshold = 5, dummy = "OTHER") {
	# code from WBRS for recoding infrequent factor levels (default is <= 5
	# observations)
	if (min(table(words)) > threshold){
		return (words)
	} else {
		return (ifelse(words %in% levels(as.factor(words))[table(words) >= threshold],
									 as.character(words), dummy) %>%
							factor %>%
							relevel(ref = dummy))
	}}
