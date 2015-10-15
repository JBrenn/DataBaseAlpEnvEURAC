# main for one Station (example B2)

# ARGUMENTS
# path2files    path to meteo files
# header.file   header file
# station       station specification, e.g. "B2"
# READvar       variables to select in function getMETEO() 
# var2geotop    variables included in GEOtop input file (GEOtop names)
# GEOtopVAR     variables included in GEOtop input file (previous names)
# pdf.name      name of pdf file, plot meteo variables time series
# plot          do plotting? PDF
# mkreg         make time series plot=FALSE,
# aggr_time     aggregation step: "h" hourly, "d" daily
# time_window   time window for GEOtop input file
# file.name     GEOtop input file name
# skipwinter    logical, set winter precipitation to NA?
# TempThresh    if skipwinter=TRUE, temperature threshold defining day with snowP
# tz            timezone specification

# FUNCTIONS
# read_station  (FUN_readStationData2zoo.R)
# getMETEO      (getMeteo.R)
# QualityData   (QualityMeteo.R)
# aggrMETEO     (arrgMeteo.R)
# FormatGEOtop  (FormatGEOtop.R)

dB_getGEOtop <- function(path2files, header.file,
                     station,
                     READvar = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation"),
                     var2geotop = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation", "CloudTrans"),
                     GEOtopVAR= c("Iprec", "WindSp", "WindDir", "RelHum", "AirT", "SWglobal", "CloudTrans"),
                     mkreg=FALSE,
                     aggr_time,
                     cut_date=FALSE,
                     time_window,
                     file.name=NULL,
                     skipwinter=FALSE, TempThresh=-2, tz="Etc/GMT-1"
                     )
{
  # source functions
  #source("FUN_readStationData2zoo.R")
  #source("getMETEO.R")
  #source("QualityMeteo.R")
  #source("aggrMeteo.R")
  #source("FormatGEOtop.R")
  
  # read data from files & choose meteo data
  data <- dB_readStationData(path = path2files, header.file = header.file, station=station)
  
  data <- data[,names(data) %in% READvar]
  
  # if time series is not strictlly regular: set missing timesteps to NA
  # e.g. for M1 | 2013-05-27 to 2013-10-08
  if (mkreg)
  {
    # sequence from start to end data by 15min
    g <- zoo(x = NA, seq(head(index(data),1),tail(index(data),1),by=1/24/4))
    data <- merge(data,g)[,1:length(cols)]
    #names(data) <- names(data)[-length(names(data))]
  }

  # DO data quality
  var <- names(data)
  
  for (i in var)
  {
    #PRECIPITATION
    if (i=="Rain")
    {
      # MIN <0
      coredata(data[,i]) <- ifelse(coredata(data[,i])<0, 0, coredata(data[,i]))
      
      # improve precipitation quality (see ESOLIP - E.Mayr)
    }
    
    # AIR TEMPERATURE
    if (i=="Temp")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i]) < (-39), NA, coredata(data[,i]))
      # MAX
      coredata(data[,i]) <- ifelse(coredata(data[,i]) > 40, NA, coredata(data[,i]))
    }
    
    # RELATIVE HUMIDITY
    if (i=="RH")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i])<0.01, NA, coredata(data[,i]))
      # MAX
      coredata(data[,i]) <- ifelse(coredata(data[,i])> 100, 100, coredata(data[,i]))
    }
    
    # SOLAR RADIATION
    if (i=="SolarRadiation")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i])< 0, NA, coredata(data[,i]))
      # MAX
      coredata(data[,i]) <- ifelse(coredata(data[,i])> 1500, NA, coredata(data[,i]))
    }
    # WIND SPEED
    if (i=="WindSpeed")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i])< 0, 0, coredata(data[,i]))
      # MAX
      coredata(data[,i]) <- ifelse(coredata(data[,i])> 50, max(coredata(data[,i], na.rm=TRUE)),
                                   coredata(data[,i]))
    }
    # WIND DIRECTION
    if (i=="WindDirection")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i])< 0, NA, coredata(data[,i]))
      # MAX
      coredata(data[,i]) <- ifelse(coredata(data[,i])> 360, NA, coredata(data[,i]))
    }
    
    # SNOW HEIGHT
    if (i=="SnowDepth")
    {
      # MIN
      coredata(data[,i]) <- ifelse(coredata(data[,i])< 0, 0, coredata(data[,i]))
      #       # MAX
      #       coredata(data[,i]) <- ifelse(coredata(data[,i])> 50, max(coredata(data[,i])), coredata(data[,i]))
    }
    
  }
  
  # DO aggregate
  var <- names(data)
  
  if (aggr_time=="h")
    # aggregation around hour: for 06:00  [05:30;06:30]
    # change to chron!
    aggr_vec <- floor(as.numeric(time(data))*24)
  # based on POSIX
  # aggr_vec <- as.POSIXct(round(as.numeric(time(data))/3600)*3600, origin="1970-01-01")
  if (aggr_time=="d")  
    aggr_vec <- as.Date(time(data))
  
  dummy <- rep(NA,length(unique(aggr_vec)))
  
  for(i in var)
  {
    if (i == "Rain")
      dummy <- cbind(dummy, coredata(aggregate(x = data[,i], by = aggr_vec, FUN = sum, na.rm=FALSE)))
    if (i == "Temp" | i == "RH" | i == "SolarRadiation" | i == "WindSpeed" | i == "WindDirection" | 
          i == "SnowDepth")
      dummy <- cbind(dummy, coredata(aggregate(x = data[,i], by = aggr_vec, FUN = mean, na.rm=TRUE)))
  }
  
  dummy <- dummy[,-1]
  colnames(dummy) <- var
  
  if (aggr_time=="h") data <- zoo(dummy, chron(unique(aggr_vec)/24))
  if (aggr_time=="d") data <- zoo(dummy,unique(aggr_vec))
  
  # Format2GEOtop
  #FormatGEOtop(data = data_aggr, GEOtopVAR = GEOtopVAR, METEOvar = var2geotop, cut_date=cut_date, window = time_window, 
  #             file.name = file.name, skipwinter=skipwinter, TempThresh=TempThresh, tz=tz)
  options(chron.year.abb = FALSE, chron.origin = c(month = 1, day = 1, year = 0000))
  
  if (cut_date)
  {
    # end & start date in chron
    start <- chron(dates. = substr(time_window[1],1,10), times. = substr(time_window[1],12,19),
                   format=c(dates = "y/m/d", times = "h:m:s"), out.format = c(dates = "m/d/y", times = "h:m:s"))
    end <- chron(dates. = substr(time_window[2],1,10), times. = substr(time_window[1],12,19), 
                 format=c(dates = "y/m/d", times = "h:m:s"), out.format = c(dates = "m/d/y", times = "h:m:s"))
    start_num <- which(as.character(time(data))==as.character(start))
    end_num   <- which(as.character(time(data))==as.character(end))
    # cut date period of interest
    data <- data[start_num:end_num,]
  }
  
  
  # skip winter precipitation
  if (skipwinter)
  {
    data$Rain <- ifelse(!is.na(data$Temp) & data$Temp<(TempThresh) & data$Rain > 0, NA, data$Rain)
    data$Rain <- ifelse(is.na(data$Temp) & data$Rain > 0 & 
                          ( as.integer(substr(time(data),2,3)) == 11 | as.integer(substr(time(data),2,3)) == 12 |
                              as.integer(substr(time(data),2,3)) == 1 | as.integer(substr(time(data),2,3)) == 2),
                        NA, data$Rain)
  }
  
  # NA value = -9999
  coredata(data) <- ifelse(is.na(coredata(data)), -9999, coredata(data))
  
  var <- names(data)
  
  dummy <- rep(NA, length(data[,1]))
  for (i in 1:length(GEOtopVAR))
  {
    geotopvar <- GEOtopVAR[i]
    if ( (geotopvar == "Iprec" & "Rain" %in% var) | (geotopvar == "WindSp" & "WindSpeed" %in% var) |
           (geotopvar == "WindDir" & "WindDirection" %in% var) | (geotopvar == "RelHum" & "RH" %in% var) |
           (geotopvar == "AirT" & "Temp" %in% var) | (geotopvar == "SWglobal" & "SolarRadiation" %in% var) | 
           (geotopvar == "CloudTrans" & "CloudTrans" %in% var) ) {
      dat <- round(coredata(data[,var2geotop[i]]),2)
      dummy <- cbind(dummy, dat)
    } else {
      dat <- rep(as.numeric(-9999), length(data[,1]))
      dummy <- cbind(dummy, dat=dat)
    }    
  }
  
  dummy <- dummy[,-1]
  
  colnames(dummy) <- GEOtopVAR
  
  # Date and Julian vector
  Datetime <- substr(chron(time(data), out.format = c(dates = "d/m/y", times = "h:m:s")),2,17)
  Date <- as.Date(time(data))
  # JDfrom0 <- as.numeric(julian(time(data), origin = as.POSIXct("0000-01-01 00:00:00", tz=tz))) + 1
  JDfrom0_days <- as.integer(julian(x = Date, origin = as.Date("0000-01-01")))
  JDfrom0 <- JDfrom0_days + as.integer(substr(Datetime,12,13))/24 + as.integer(substr(Datetime,15,16))/60/24
  JDfrom0 <- formatC(JDfrom0, format = "f", digits = 6)
  
  df <- data.frame(Date=Datetime, JDfrom0=JDfrom0, dummy)
  
  if (!is.null(file.name))
    write.table(x = df, file = file.name, sep = ",", quote = FALSE, row.names=FALSE)
  
  df <- apply(df[,-1],2,as.numeric)
  
  return( zoo(df,chron(time(data)) ) )
}