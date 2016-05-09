# get meteorological data for e.g. GEOtop - input

# ARGUMENTS
# path2files    path to meteo files
# header.file   header file
# station       station specification
# cols          column names to extract
# plot          should data be ploted?
# pdf.name      name of output pdf, if plot=TRUE
# mkreg         make regular zoo object

#Date,JDfrom0,Iprec,WindSp,WindDir,RelHum,AirT,Swglobal
#time(data),julian(x,origin=as.POSIXct("1960-01-01 00:00:00")),Rain,WindSpeed,WindDirection,RH,Temp,SolarRadiation

dB_getMETEO <- function(path2files, header.file, station, station_nr,
                        cols = c("Tip_Precip_Tot","Wind_Speed","Wind_Dir","RH","T_Air","Net_Rad_Sw"), 
                        #cols = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation"), 
                        plot=FALSE, pdf.name, mkreg=FALSE)
{
  # source functions
  #source("H:/Projekte/HiResAlp/06_Workspace/BrJ/04_R_data_analyses/data_base/FUN_readStationData2zoo.R")
  
  #require("zoo")
  station <- paste(station,station_nr,sep="")
  
  data <- dB_readStationData(path = path2files, header.file = header.file, station=station)
  
  data <- data[,names(data) %in% cols]
  
  if (plot) {
    pdf(pdf.name)
    donotplot <- c(grep(pattern = "TS", names(data)), grep(pattern = "SWC", names(data)))
    plot(data[,-donotplot], main=pdf.name)
    dev.off()
  } 
  
  # if time series is not strictlly regular: set missing timesteps to NA
  # e.g. for M1 | 2013-05-27 to 2013-10-08
  if (mkreg)
  {
    #require("chron")
    g <- zoo(x = NA, seq(head(index(data),1),tail(index(data),1),by="15 min"))
    data <- merge(data,g)[,1:length(cols)]
    #names(data) <- names(data)[-length(names(data))]
  }
  
  return(data)
}

