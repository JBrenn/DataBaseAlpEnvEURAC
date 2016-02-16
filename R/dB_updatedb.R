
# library("DataBaseAlpEnvEURAC")
# library(zoo)
# library(chron)


dB_updatedb <- function(stations = c("B1","B2","B3","P2","M1","M2","M3","M4","M5","M6","M7"), 
                        variables = "METEO",
                         path2data = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/", 
                         inCloud = "/home/jbr/ownCloud/data/",
                        return_data = FALSE)
{
  # connect to db in data folder of project
  db = dbConnect(SQLite(), dbname=file.path(inCloud,paste(variables,".sqlite",sep="")))
  
  out <- list()
  
  for (i in stations)
  {
    stationchr <- substr(i, 1, nchar(i)-1)
    
    if (stationchr == "XS") stationchr <- "S"
    
    stationnr  <- as.integer(substr(i, nchar(i), nchar(i)))
    
    print(paste("updating ", variables, " data of station", i, sep=" "))
    
    path2files = file.path(path2data,stationchr,i)
    header.file = file.path(path2data,stationchr,paste("header_",i,".txt",sep=""))
    
    if (variables == "SWC") {
      data <- dB_getSWC(path2files, header.file, station = stationchr, station_nr = stationnr, calibrate = F, 
                        minVALUE = 0, maxVALUE = 1, aggregation = "n")
      
      if(any(names(data)=="core5")) names(data)[which(names(data)=="core5")] <- "SWC_A_z5"
      if(any(names(data)=="core20")) names(data)[which(names(data)=="core20")] <- "SWC_A_z20"
    }

    if (variables == "METEO") {
      data <- dB_getMETEO(path2files, header.file, station = stationchr, station_nr = stationnr)
    }
    
    # remove data with NA date
    data <- data[!is.na(index(data))]
    
    if(!is.regular(data))
    {
      # make regular
      g <- zoo(x = NA, seq(head(index(data),1),tail(index(data),1),by=times("00:15:00")))
      data <- merge(g,data)[,-1]
    }
    
    df <- data.frame(datetime=index(data),coredata(data))
    
    # update litesql
    dbWriteTable(conn=db, name=i,
                 value=df, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
    
    out[[i]] <- data
  }
  
  # list tables in db
  print("Tables in data base:")
  print(dbListTables(db))
  
  dbDisconnect(db)
  
  if (variables=="SMC")
  {
    if (any(row.names(installed.packages())=="SMCcalibration") & !is.null(inCloud)) 
    {
      print("moving database swc.sqlite into data folder of the package SMCcalibration")
      pkg_path <- path.package("SMCcalibration") 
      system(paste("mv", file.path(inCloud,"swc.sqlite"), file.path(pkg_path,"data","swc.sqlite")))
    }
  }
 
  if (return_data) return(out)
}
