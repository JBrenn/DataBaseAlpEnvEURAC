# function read station data

#ARGUMENTS
# path        path to data files
# header.file path and name headerfile
# station    
#e.g.
#path <- "H:/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B1/"
#header.file <- "H:/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B1.txt"

dB_readStationData <- function(path, header.file, station)
{
# load libraries
#  require(zoo)
#  require(chron)
  
# supress chron year abbreviation  
  options(chron.year.abb = FALSE)
  
# get header
  header <- as.character(read.table(header.file, header=FALSE)[,1])
  
# get file names  
  files <- dir(path)

  station_gen <- substr(station,1,nchar(station)-1)

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
    # whole data frame
    if (i=="B3_2000_YEAR_2014.csv" | i=="B3_2000_YEAR_2015.csv") {
      dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, dec=".",
                         na.strings=c("NaN","7777","-888.88","-999", "NAN"))
      dummy <- dummy[,1:dim(data)[2]]
    } else {
      if (i=="B3_2000_YEAR_2010.csv" | i=="B3_2000_YEAR_2012.csv") {
        dummy <- read.csv2(file.path(path,i), skip=skip, header=FALSE, 
                           na.strings=c("NaN","7777","-888.88","-999", "NAN"))
        
      } else {
        if (i=="I1_YEAR_2014.csv") {
          dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                            na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
          dummy <- dummy[,-1]
          names(dummy) <- paste("V", 1:length(dummy), sep="")
        } else {
          if (i=="M1_total_2014.csv") {
            dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                              na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
            dummy <- dummy[,1:dim(data)[2]]  
          } else {
            dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                              na.strings=c("NaN","7777","-888.88", "-999", "NAN"))
          }  
          
       
        }
        
      }
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

  # create zoo object
  zoo.data <- zoo(x=data, order.by=datetime)
  names(zoo.data) <- header[-c(1:date_col)]
  
  return(zoo.data)
}