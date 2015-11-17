# get snow depth data

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

dB_getSH_par <- function(path2files, header.file, station)
{
  # source functions
  #source("H:/Projekte/HiResAlp/06_Workspace/BrJ/04_R_data_analyses/data_base/FUN_readStationData2zoo.R")
  
  #require("zoo")
  
  data <- dB_readStationData(path = path2files, header.file = header.file, station=station)
  
  data_sh <- data[,c(grep(pattern = "SnowDepth", names(data)), grep(pattern = "PARsoil", names(data)))]
  data_sh <- data_sh[,-c(grep(pattern = "Std", names(data_sh)))]
  
  # get snow cover periods from PARsoil data
  data_par <- data_sh[,-1]
  coredata(data_par) <- ifelse(coredata(data_par)<0 | coredata(data_par)>2700,NA,coredata(data_par)) 
  
  if (station="B2")
  {
    data_par <- window(data_par, start = as.chron(as.Date("2010-06-01")))
  }
  
  data_par_mean <- zoo(rowMeans(data_par, na.rm=F), time(data_par))
  data_par_mean_rollmean <- rollmean(x = data_par_mean, k = 4, fill = NA)
  
  sh <- window(data_sh[,1], start = time(data_par_mean)[1], end = tail(time(data_par_mean),1))
  sh <- ifelse(data_par_mean < 40 | !is.na(data_par_mean), coredata(sh), NA)
  
  return(list(sh, data_par))
}
