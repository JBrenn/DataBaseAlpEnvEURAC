# AUTHOR: Johannes Brenner, Institute for Alpine Environments
# DATE.VERSION: 19.08.2014 V1

# PURPOSE
# read ZRX data files (province Bozen, WISKI, batch download) with function readZRX
# general quality check | min - max variable dependent
# & hourly aggregation (mean - sum) possible
# AND
# return .csv file for each station with available variables

# required libraries: zoo
# required source:    readZRX.R

# ARGUMENTS
# files       ZRX file names (absolute paths)
# write.csv   logical, if TRUE .csv files for each station are written
# output_path path to which .csv files are writte
# do.hourly   logical, if TRUE data gets hourly aggregated
# do.quality  logical, if FALSE general quality check is performed (min - max)

# VALUES
# output .csv files containing available variables for each station
# list containing zoo objects for each station

dB_readZRX2station <- function(files, write_csv=FALSE, output_path, do.hourly=FALSE, do.quality=FALSE, chron=TRUE, 
                               multivar=FALSE, multistation=FALSE, saveRData=FALSE)
  
  {
    # source function readZRX
    #source("readZRX")
  
    # dummies for station names, data and meta data
    stnames <- c()
    out_data <- list()
    out_metadata <- list()
    
    # dummy for station data
    station_data <- list()
    
    # correct list to exclude void file
    empty_file <- list()
    for (f in files)
    {
      if (file.info(f)$size == 0)
          {
            empty_file[[length(empty_file)+1]] <- f
            #empty_file = list(empty_file, c=f)
            files <- files[files != f]
          }
    }
    
    if (write_csv) {
        write.csv(as.data.frame(empty_file), file = file.path(output_path, paste("empty_file_list",".csv",sep="")), quote=F, row.names = F, col.names = F)
    } else {
        print(as.character(empty_file), quote=T)
    }
    
    if (length(files) == 0)
    {
      print("All given files are empty. Execution interrupted.")
      stop()
    }
    
    # read data via loop over files
    for (i in files)
    {
      print(paste("file", i, sep=" "))
      if (multivar) {
        out <- dB_readZRX(i, do.hourly = do.hourly, do.quality = do.quality, chron = chron, multivar = multivar)
        for (st in unique(out$meta[,"st_id"]))
        {
          out_data[[paste("st",st,sep="")]] <- out$data[out$meta[,1]%in%st]
          out_metadata[[paste("st",st,sep="")]] <- out$meta[out$meta[,1]%in%st,]
          stnames <- c(stnames, paste("st",st,sep=""))
          
          # merge variables in one zoo object
          
          if (length(out_data[[paste("st",st,sep="")]]) > 1) {
            dummy <- out_data[[paste("st",st,sep="")]][[1]]
            
            for (t in 2:length( out_data[[paste("st",st,sep="")]] ))
              dummy <- cbind(dummy, out_data[[paste("st",st,sep="")]][[t]])
            
          } else {
            dummy <- out_data[[paste("st",st,sep="")]][[1]]
          }
          names(dummy) <- substr(names(out_data[[paste("st",st,sep="")]]), 8,nchar(names(out_data[[paste("st",st,sep="")]])))
          
          # write.csv
          # write .csv file containing station data
          if (write_csv)
          {
            #STinMetadata <- which(substr(i,3,nchar(i))==metadata[,"st_id"])
            if (do.hourly==T & as.integer(unique(out_metadata[[paste("st",st,sep="")]][,"time_agg"])) < 60){
              output_filename <- paste("st", st, "_60", sep="")
            } else {
              output_filename <- paste("st", st, "_", unique(out_metadata[[paste("st",st,sep="")]][,"time_agg"]), sep="")
            }
            if ( as.integer(unique(out_metadata[[paste("st",st,sep="")]][,"time_agg"])) <= 60) {
              df <- data.frame(date = format(time(dummy), "%d/%m/%Y %H:%M:%S"), coredata(dummy))
              write.csv(x = df)
            } else {
              write.zoo( x = dummy, file = file.path(output_path, paste(output_filename,".csv",sep="")), 
                         row.names=F, col.names=T, sep=",", quote=F, index.name="date") 
            }
           
          }
          
          station_data[[paste("st",st,sep="")]] <- dummy
        }
        
      } else {
        out <- dB_readZRX(i, do.hourly = do.hourly, do.quality = do.quality, chron = chron, multivar = multivar)
        out_data[[substr(i,1,nchar(i)-4)]] <- out[[1]]
        out_metadata[[substr(i,1,nchar(i)-4)]] <- out[[2]]
        stnames <- c(stnames, names(out_data[[substr(i,1,nchar(i)-4)]]))
      }
      
    }
  
    if (!multivar) {
      
      # get unique station IDs
      stations <- unique(stnames)
      
      # preperation for dummy with minimal time frame 
      t <- lapply(X = out_data, FUN = function(x){
        lapply(X = x, FUN = function(x){
          diff(range(time(x)))
        })
      })
      t <- lapply(t, unlist)
      min1 <- lapply(t, which.min)
      min2 <- which.min(unlist(lapply(t, which.min)))
      
      # loop over unique station vector
      for (i in stations)
      {
        # dummy for specific station and variable available for this station
        dummy <- zoo(NA, time(out_data[[min2]][[min1[[min2]]]]))
        name_spec <- c()
        
        # loop over variables
        for (dat in names(out_data))
        {
          #get meta data for variable dat
          metadata <- out_metadata[[dat]]
          
          #get data for variable dat and station i
          data <- out_data[[dat]]
          
          if ( any(names(data)==i) ){
            st_data <- data[[i]]
            
            dummy <- merge(dummy, st_data)
            name_spec <- c(name_spec, TRUE)
          } else {
            name_spec <- c(name_spec, FALSE)
          }
        }
        dummy <- dummy[,-1]
        
        # name coloums of zoo object
        names(dummy) <- names(out_data)[name_spec]
        
        # write .csv file containing station data
        if (write_csv)
        {
          #STinMetadata <- which(substr(i,3,nchar(i))==metadata[,"st_id"])
          if (do.hourly==T & as.integer(unique(metadata[,"time_agg"])) < 60){
            output_filename <- paste(i, "60", sep="_")
          } else {
            output_filename <- paste(i, unique(metadata[,"time_agg"]), sep="_")
          }
          write.zoo( x = dummy, file = file.path(output_path, paste(output_filename,".csv",sep="")), 
                     row.names=F, col.names=T, sep=",", quote=F, index.name="date")
        }
        
        # save data in station data list
        station_data[[i]] <- dummy
      }
    }
    
    if (write_csv) 
    {
      if (length(files)==1 & !multistation) {
        filen <- paste("meta_",stnames,".csv",sep="")
        write.csv(out_metadata[[1]], file.path(output_path,filen), row.names=F, quote = F)
      } else {
        for (m in names(out_metadata))
        {
          filen <- paste("meta_",m,".csv",sep="")
          write.csv(out_metadata[[m]], file.path(output_path,filen), row.names=F, quote = F)
        }
      }
    
    }
    
    if (saveRData)
    {
      save(list = "station_data", file = "data.RData")
    }
    
    # return function output
    return(station_data)

  }
