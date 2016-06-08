# AUTHOR: Johannes Brenner, Institute for Alpine Environments
# DATE.VERSION: 19.08.2014 V1.1

# PURPOSE
# read ZRX data file (province Bozen, WISKI, batch download)
# general quality check | min - max variable dependent
# & hourly aggregation (mean - sum)

# required libraries: zoo, chron

# ARGUMENTS
# file        ZRX file name (absolute path)
# do.hourly   logical, if TRUE data gets hourly aggregated
# do.quality  logical, if FALSE general quality check is performed (min - max)

dB_readZRX <- function(file, do.hourly=FALSE, do.quality=FALSE, chron=TRUE, multivar=FALSE)
{
  # load zoo library
  # require("zoo")
  
  # open connection
  dummy    <- readLines(con=file, n = -1)
  # get begining of meta data (file contains data for several stations)
  start_st <- c(grep(substr(dummy[1],1,22),dummy),length(dummy)+1)
  
  # dummies for data and metadata
  data_list <- list()
  meta_mat  <- c()
  
  # loop over start_st
  for (i in 1:(length(start_st)-1))
  {
    # get data for specific station
    data <- encodeString(dummy[ start_st[i]:(start_st[i+1]-1) ])
 
    #-----
    # get METADATA
    header <- data[ c(grep("#", data)) ]
    
    # get station info (name, id)
    st_name <- strsplit( x = header[ grep("#SNAME",header) ], split = ";")[[1]][1]
    st_name <- substr(st_name, 7, nchar(st_name))
    
    rexchange <- strsplit( x = header[ grep("#REXCHANGE",header) ], split = ";")[[1]][1]
    st_id <- substr(rexchange, 11, 14)
    
    # get variable info
    var_name <- substr(rexchange, 15, nchar(rexchange))
    
    # get time step in minutes
    if (length( grep("5A", var_name ) )==1) time_scale <- 5
    if (length( grep("10A", var_name) )==1) time_scale <- 10
    if (length( grep("60A", var_name) )==1) time_scale <- 60
    if (length( grep("TAG", var_name) )==1) time_scale <- 60*24
    
    # meta data vector
    meta_mat <- rbind(meta_mat, c(st_id=st_id, st_name=st_name, var_name=var_name, 
                                  time_agg=as.character(time_scale)) )
    #-----
    # get DATA
    
    # ommit metadata
    data_ts <- data[ -c(grep("#", data)) ]
    # split data character vector in colums
    data_ts <- strsplit(x = data_ts, split = " ")
    data_ts <- t(sapply(X = data_ts, FUN = function(x) x[c(1,2)]))
    
    # daily data
    if (time_scale==60*24) {
      
      # create date vector
      year <- substr(data_ts[,1],1,4); month <- substr(data_ts[,1],5,6); day <- substr(data_ts[,1],7,8)
      date_chr <- paste(year, "-", month, "-", day, sep="")
      date <- as.Date(x = date_chr, format = "%Y-%m-%d") 
      
      # create zoo object
      data_zoo <- zoo( x = as.numeric(data_ts[,2])[!is.na(date) & c(diff(date)!=0,T)], 
                       order.by = date[!is.na(date) & c(diff(date)!=0,T)] )
      
      # create dummy regular zoo object
      data_zooreg <- zooreg(data = NA, order.by = seq( from = time(data_zoo)[1],to = tail(x = time(data_zoo),n = 1), 
                                                       by = 1) )
      # combine both zoo objects 
      data_zooreg <- zoo(cbind(data_zoo, data_zooreg)$data_zoo)
      
    # no daily data  
    } else {
      # create datetime vector
      year <- substr(data_ts[,1],1,4); month <- substr(data_ts[,1],5,6); day <- substr(data_ts[,1],7,8)
      hour <- substr(data_ts[,1],9,10); min  <- substr(data_ts[,1],11,12); sec <- substr(data_ts[,1],13,14)
      
      if (chron) {
        date_chr <- paste(year, "-", month, "-", day, sep="")
        time_chr <- paste(hour, ":", min, ":", sec, sep="")
        date <- chron(dates. = date_chr, times. = time_chr, 
                      format = c(date="y-m-d", time="h:m:s"))
      } else {
        datetime <- paste(year, "-", month, "-", day, " ", hour, ":", min, ":", sec, sep="")
        date <- as.POSIXct( strptime(x = datetime, format = "%Y-%m-%d %H:%M:%S") )
      }
    
      # create zoo object
      data_zoo <- zoo(x = as.numeric(data_ts[,2][!is.na(date)]), order.by = date[!is.na(date)])
      
      # create dummy regular zoo object
      if (chron){
        if (time_scale==60) {
          data_zooreg <- zooreg(data = NA, order.by = seq( from = time(data_zoo)[1],to = tail(x = time(data_zoo),n = 1), 
                                                           by = times("01:00:00") ) )
                                                           
        } else {
          data_zooreg <- zooreg(data = NA, order.by = seq( from = time(data_zoo)[1],to = tail(x = time(data_zoo),n = 1), 
                                                           by = times( paste("00:",time_scale,":00",sep="") ) ) ) 
        }
      } else {
        data_zooreg <- zooreg(data = NA, order.by = seq( from = time(data_zoo)[1],to = tail(x = time(data_zoo),n = 1), 
                                                         by = time_scale*60) )
      }
      
      # combine both zoo objects
      data_zooreg <- zoo(cbind(data_zoo, data_zooreg)$data_zoo)
    }
    
    #-----
    # possible simple quality check
    if (do.quality)
    {
      # data quality check
      # relative air humidiy LF
      if (grepl("LF", var_name)) {
        data_zooreg <- ifelse(data_zooreg > 100, 100, data_zooreg)
        data_zooreg <- ifelse(data_zooreg < 0, 0, data_zooreg)
      }
      # air temperature LT
      if (grepl("LT", var_name)) {
        data_zooreg <- ifelse(data_zooreg > 50, NA, data_zooreg)
        data_zooreg <- ifelse(data_zooreg < -50, NA, data_zooreg)
      }
      # air temperature TD
      if (grepl("TD", var_name)) {
        data_zooreg <- ifelse(data_zooreg > 25, NA, data_zooreg)
        data_zooreg <- ifelse(data_zooreg < -40, NA, data_zooreg)
      }
      # for WG & WD quality check lock at 
        # Jimenez et al. (2010) - random, systematic, rough errors
        #                       - Manipulation errors - Limits consistency (0-30m/s, 0-360°)
        #                       - Temporal consistency (ABNORMALLY LOW VARIATIONS & ABNORMALLY HIGH VARIATIONS)
        # Chvez-Arroyo and Probst (2013)
      
      # wind velocity WG
      if (grepl("WG", var_name)) {
        data_zooreg <- ifelse(data_zooreg > 35, NA, data_zooreg)
        data_zooreg <- ifelse(data_zooreg < 0, NA, data_zooreg)
    
      }
      # wind direction WD
      if (grepl("WD", var_name)) {
        data_zooreg <- ifelse(data_zooreg > 360, NA, data_zooreg)
        data_zooreg <- ifelse(data_zooreg < 0, NA, data_zooreg)
      }
      # Precipitation NN
      if (grepl("NN", var_name)) {
        data_zooreg <- ifelse(data_zooreg < 0, NA, data_zooreg)
      }
    }
   
    #-----
    # possible hourly aggregation
    if (do.hourly)
    {
      if (chron) {
        hour <- dB_trunc.minutes(x = time(data_zooreg), n.minutes = 60)
      } else {
        hour <- as.POSIXct( strptime(format(time(data_zooreg), "%Y-%m-%d %H"), format= "%Y-%m-%d %H") )
      }
      # hourly aggregation for precipitation (sum)
      if (time_scale<60 & length(grep("N", var_name))==1)
      {
        data_zooreg <- aggregate(x = data_zooreg, by = hour, FUN = function (x) { if (any(is.na(x))) { 
          y <- NA } else {
            y <- sum(x)
          } 
          return(y)
        } )
      }
      
      # hourly aggregation for other variables (mean)
      if (time_scale<60 & length(grep("N", var_name))!=1)
      {
        data_zooreg <- aggregate(x = data_zooreg, by=hour, FUN=mean, na.rm=TRUE)
      } 
    }
   
    # save data in output list
    if (multivar) {
      data_list[[paste("st",st_id,"_",var_name,sep="")]] <- data_zooreg
    } else {
      data_list[[paste("st",st_id,sep="")]] <- data_zooreg
    }
    
  }
  
  #return function output
  return(list(data=data_list, meta=meta_mat))
}
