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

dB_getMETEO <- function(
                        path2data = "/media/alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia", 
                        station,
                        cols = c("Tip_Precip_Tot","Wind_Speed","Wind_Dir","RH","T_Air","SR_Sw"), 
                        plot=FALSE, pdf.name)
{
  # source functions
  #source("H:/Projekte/HiResAlp/06_Workspace/BrJ/04_R_data_analyses/data_base/FUN_readStationData2zoo.R")
  
  station_nr <- as.integer(substr(station, nchar(station), nchar(station)))
  
  station_  <- substr(station, 1, nchar(station)-1)
  
  if (station_ == "XS") station_ <- "S"
  
  path2files = file.path(path2data,station_,station)
  header.file = file.path(path2data,station_,paste("header_",station,".txt",sep=""))
  
  data <- dB_readStationData(path = path2files, header.file = header.file, station = station)
  
  data <- data[,names(data) %in% cols]
  
  if (plot) {
    pdf(pdf.name)
    donotplot <- c(grep(pattern = "TS", names(data)), grep(pattern = "SWC", names(data)))
    plot(data[,-donotplot], main=pdf.name)
    dev.off()
  } 
  
  return(data)
}

