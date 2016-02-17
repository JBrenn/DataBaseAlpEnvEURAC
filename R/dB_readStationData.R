# function read station data

#ARGUMENTS
# path        path to data files
# header.file path and name headerfile
# station    
#e.g.
# path <- "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/P3/"
# header.file <- "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/header_P3.txt"

dB_readStationData <- function(path, header.file, station)
{
# load libraries
#  require(zoo)
#  require(chron)
  
# supress chron year abbreviation  
  options(chron.year.abb = FALSE)
  
# get file names  
  files <- dir(path)
  
# get header
  header <- as.character(read.table(header.file, header=FALSE, sep=",")[,1])
  header_org <- header

  station_gen <- substr(station,1,nchar(station)-1)

  if (station_gen=="SF") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"
  }
  if (station_gen=="B") {
    skip <- 4; date_col=1; tz="Etc/GMT-2"
  }
  if (station_gen=="P"| station_gen=="I") {
    skip <- 4; date_col=1; tz="Etc/GMT-1"
  }
  if (station_gen=="M" | station_gen=="S") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"
  }
  if (station=="S2") {
    skip <- 1; date_col=2; tz="Etc/GMT+2"
  }
 
# read data 
  data <- rep(NA,length(header))
  datetime <- chron(dates.  = "2012-02-02", times. = "01:00:00", 
                    format= c(dates = "y-m-d", times = "h:m:s"))

# do not work with POSIX as timezone is needed, better solution chron
#datetime <- as.POSIXct(strptime(x="2012-02-02 00:00", format="%Y-%m-%d %H:%M", tz=tz))

  for (i in files)
  {
    # change header where needed
    # M2 
    if (i== "M2 Station total_2014_07_07_TO_2014_11_14.csv"| i=="M2 Station total_2014_11_14_TO_2015_07_09.csv") {
      header.file_ <- paste(substr(header.file, 1, nchar(header.file)-13), "header_M2_2015.txt", sep="")
      header <- as.character(read.table(header.file_, header=FALSE)[,1])
    }
    
    # whole data frame
    if (i=="P3_YEAR_2016.csv") {
      dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                        na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
      dummy$V41 <- NA
      dummy$V42 <- NA
    } else if (i=="B3_2000_YEAR_2014.csv" | i=="B3_2000_YEAR_2015.csv" | 
        i=="B1_1000_YEAR_2016.csv" | i=="B2_1500_YEAR_2016.csv" | i=="B3_2000_YEAR_2016.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, dec=".",
                           na.strings=c("NaN","7777","-888.88","-999", "NAN"))
        dummy <- dummy[,1:dim(data)[2]]
    } else if (i=="B3_2000_YEAR_2010.csv" | i=="B3_2000_YEAR_2012.csv") {
        dummy <- read.csv2(file.path(path,i), skip=skip, header=FALSE, 
                           na.strings=c("NaN","7777","-888.88","-999", "NAN"))
    } else if (i=="I1_YEAR_2014.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                          na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
        dummy <- dummy[,-1]
        names(dummy) <- paste("V", 1:length(dummy), sep="")
    } else if (i=="M1_total_2014-2015.csv" | i=="P3_YEAR_2015.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                            na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
        dummy <- dummy[,1:length(header)]
    } else {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                          na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
    } 
    
    if (exists("header.file_")) {
      # reorder data
      names(dummy) <- header
      dummy <- dummy[header_org]
      names(dummy) <- names(data)
      rm(header.file_)
    }
    
    data <- rbind(data,dummy)
    
    # extract date and time
    
  # differing datetime formats
    # "%Y-%m-%d %H:%M:%S"
    if (substr(as.character(dummy[1,date_col]),5,5)=="-" & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%Y-%m-%d %H:%M", tz=tz)) )
    # "%Y-%m-%d %H:%M"
    if (substr(as.character(dummy[1,date_col]),5,5)=="-" & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%Y-%m-%d %H:%M", tz=tz) ))
    # "%Y/%m/%d %H:%M"
    if (substr(as.character(dummy[1,date_col]),3,3)=="/" & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "d/m/y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d/%m/%Y %H:%M", tz=tz) ))
    # "%Y/%m/%d %H:%M:%S"
    if (substr(as.character(dummy[1,date_col]),3,3)=="/" & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "d/m/y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d/%m/%Y %H:%M", tz=tz)) )
    
    if (substr(as.character(dummy[1,date_col]),3,3)=="." & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "d.m.y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d.%m.%Y %H:%M", tz=tz) ))
    if (substr(as.character(dummy[1,date_col]),3,3)=="." & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "d.m.y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d.%m.%Y %H:%M", tz=tz)) )
  }
  
  datetime <- datetime[-1]
  data <- data[-1,-c(1:date_col)]

  # create regular zoo object
  zoo.data <- zoo(x=data, order.by=datetime)
  names(zoo.data) <- header[-c(1:date_col)]
  
  return(zoo.data)
}