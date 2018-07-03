##########################################################################################
# COMPUTATION TIMES
##########################################################################################
rm(list = ls(all=TRUE)); gc()
get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "osx" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}
OS<-get_os()
if (OS=="osx") {
	pth<-"~/OneDrive - University of Helsinki/" 
}
if (OS=="win") {
	pth<-"D:/HY-data/NORBERG/OneDrive - University of Helsinki/" 
}
SETT<-paste(pth,"bakeoff/pipeline/SCRIPTS/settings.r",sep="")

source(SETT)
setwd(WD)

require(rmatio)
require(R.matlab)
	
for (sz in 1:2) {

	for (d in 1:length(Sets)) {
		set_no <- Sets[d]

		comtimes<-list()
		for (m in 1:length(mod_names3)) {
	
			if (m <= 20) {
				if (m > 16 & m < 19) {
					load(file=paste(FD2,set_no,"/comTimes_",mod_names3[m],"_",dataN[sz],".RData",sep=""))
					comtimes[[m]]<-as.numeric(comTimes, units = "mins")	
				} else {
				load(file=paste(FD,set_no,"/comTimes_",mod_names3[m],"_",dataN[sz],".RData",sep=""))
				comtimes[[m]]<-as.numeric(comTimes, units = "mins")	
				}
			}
			if (m > 20 & m <= 22) {
				tmp<-read.mat(filename=paste(FD,set_no,"/ssHMSC/compTime_",Sets[d],"_",mod_names3[m],"_",dataN[sz],".mat",sep=""))
				comtimes[[m]]<-as.numeric(tmp)/60
			}	
			if (m > 22) {
				tmp<-read.mat(filename=paste(FD,set_no,"/compTime_hmsc_",Sets[d],"_",m-22,"_",dataN[sz],".mat",sep=""))
				comtimes[[m]]<-as.numeric(tmp)/60
			}	
		}
		save(comtimes, file=paste(RD2,set_no,"/comtimes_",dataN[sz],".RData",sep=""))
	}
}
