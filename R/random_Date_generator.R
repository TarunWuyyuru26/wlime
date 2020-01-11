
# Create Random Dates
rand.day.time <- function(day.start = '1-1-2000', day.end = '31-12-2019', size = 0, rndm_NA = 0){
  dat_smpl = sample(seq(as.Date(day.start), as.Date(day.end), by="day"), size = size)
  ind <- which(dat_smpl %in% sample(dat_smpl, rndm_NA))
  dat_smpl[ind]<-NA
  dat_smpl
}