
# Generating NAs in the Dataset
gen_NAs <- function(ipdata, perc_NA = 0.1){
  return(as.data.frame(lapply(ipdata, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, perc_NA)))))))
}