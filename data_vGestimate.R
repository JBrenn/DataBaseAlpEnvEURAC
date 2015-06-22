# get data B2 for vanGenuchten parameter estimation

library("DataBaseAlpEnvEURAC", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.1")
library(chron)
library(zoo)

# soil water pressure head (hPa, cm water column)
SWP <- dB_getSWP(path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B2/", 
                 header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B2.txt", 
                 aggregation="n", minVALUE = 0, maxVALUE = 2000)

# soil water content
SWC <- dB_getSWC(path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B2/",
                 header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B2.txt", 
                 aggregation="n", remove_freezing=TRUE, station="B", station_nr=2)

names(SWP)
names(SWC)

#sensor CSn_z5 / NO
# CSn_z5 <- merge(SWC[,2], SWP[,1])
# plot(CSn_z5)

#sensor CSt_z5 / part 2010-01 - 2012-01
CSt_z5 <- merge(SWC[,10], SWP[,2])
CSt_z5_use <- window(CSt_z5, start = chron(as.numeric(as.Date("2010-01-01"))), end = chron(as.numeric(as.Date("2012-01-01"))))

#sensor CI_z5 / OK
CI_z5 <- merge(SWC[,4], SWP[,2])

#data5 <- rbind(coredata(CSt_z5_use[!is.na(CSt_z5_use[,1]) & !is.na(CSt_z5_use[,2]),]), coredata(CI_z5[!is.na(CI_z5[,1]) & !is.na(CI_z5[,2]),]))
#data5 <- coredata(CSt_z5_use[!is.na(CSt_z5_use[,1]) & !is.na(CSt_z5_use[,2]),])
data5 <- coredata(CI_z5[!is.na(CI_z5[,1]) & !is.na(CI_z5[,2]),])
data5_cleared <- data5[data5[,1]>.2 & log(data5[,2])>2.5,]

plot(x = data5[,2], y = data5[,1], log="x")
plot(x = data5_cleared[,2], y = data5_cleared[,1], log="x")

#sensor CSn_20 / OK
CSn_z20 <- merge(SWC[,15], SWP[,4])

#sensor CSt_20 / NO

#sensor CI_20 / OK
CI_z20 <- merge(SWC[,13], SWP[,6])

#data20 <- rbind(coredata(CSn_z20[!is.na(CSn_z20[,1]) & !is.na(CSn_z20[,2]),]), coredata(CI_z20[!is.na(CI_z20[,1]) & !is.na(CI_z20[,2]),]))
data20 <- coredata(CSn_z20[!is.na(CSn_z20[,1]) & !is.na(CSn_z20[,2]),])
#data20 <- coredata(CI_z20[!is.na(CI_z20[,1]) & !is.na(CI_z20[,2]),])
data20_cleared <- data20[data20[,1]>.11,]

plot(x = data20[,2], y = data20[,1], log="x")
plot(x = data20_cleared[,2], y = data20_cleared[,1], log="x")

plot(SWP)
plot(SWC)