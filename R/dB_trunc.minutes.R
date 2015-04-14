# truncate chron object
# for usage in aggrgation of zoo-objects with chron date-times

dB_trunc.minutes <- function (x, n.minutes) 
{
  if (!inherits(x, "times")) 
  x <- as.chron(x = x)
  x <- as.numeric(x)
  sec <- round(24 * 3600 * abs(x - floor(x)))
  h <- (sec%/%(n.minutes*60))/(60/n.minutes)
  hour <- as.integer(h)
  minutes <- (h %% hour) * 60
  chron(dates=chron::dates(x), times=times(paste(hour, minutes, "00", sep=":")))
}