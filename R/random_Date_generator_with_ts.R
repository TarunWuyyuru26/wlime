
# Create Random Dates with Time Stamps
rand.day.time.stamp <- function(day.start = '1-1-2000', day.end = '31-12-2019', size = 0, rndm_NA = 0){
  dayseq <- seq.Date(as.Date(day.start),as.Date(day.end),by="day")
  dayselect <- sample(dayseq,size,replace=TRUE)
  hourselect <- sample(1:24,size,replace=TRUE)
  minselect <- sample(0:59,size,replace=TRUE)
  dat_stmp_op = as.POSIXlt(paste(dayselect, hourselect,":",minselect,sep=""))
  
  ind <- which(dat_stmp_op %in% sample(dat_stmp_op, rndm_NA))
  dat_stmp_op[ind]<-NA
  dat_stmp_op
}