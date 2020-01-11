
# Special Character finder in the dataset
special_char_finder <- function(ipdata, special_chars){
  for (i in special_chars) {
    cat(i,"=>",any(sapply(ipdata, function(x){i %in% x})),"\n")
  }
}