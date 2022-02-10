filter.infrequent <- function(words, threshold = 5, dummy = "OTHER") {
  # code from WBRS for recoding infrequent factor levels (default is <= 5
  # observations)
  # require(magrittr)
  if (min(table(words)) > threshold){
    return (words)
  } else {
    output <- ifelse(words %in% levels(as.factor(words))[table(words) > threshold],
                     as.character(words), dummy)
    output <- factor(output)
    output <- relevel(output, ref = dummy)
    return (output)
  }}
