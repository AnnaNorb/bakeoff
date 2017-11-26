##########################################################################################
# GENERALIZED ADDITIVE MODELS PREDICTIONS
##########################################################################################

require("mgcv")

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	for (m in 1:4) {

		if (m==1) { 
			load(file=paste(FD,set_no,"/gams_",j,"_",dataN[sz],".RData",sep=""))
			}
		if (m==2) { 
			load(file=paste(FD,set_no,"/gams2_",j,"_",dataN[sz],".RData",sep="")) 
			gams<-gams2
			}
		if (m==3) { 
			load(file=paste(FD,set_no,"/gams_spat1_",j,"_",dataN[sz],".RData",sep=""))
			gams<-gams_spat1
			}
		if (m==4) {
			load(file=paste(FD,set_no,"/gams_spat2_",j,"_",dataN[sz],".RData",sep=""))
			gams<-gams_spat2
			}

		isNull<-unlist(lapply(gams, is.null))
		gam_preds<-vector("list",nsp)

		for (i in c(1:nsp)[!isNull]) {
			if (m==3) { 
				gam_preds[[i]]<-predict(gams[[i]]$gam,newdata=DD_v[[j]][[i]],type="link")							
			} else { 
				gam_preds[[i]]<-predict(gams[[i]],newdata=DD_v[[j]][[i]],type="link") 
			}
		}
		for (i in c(1:nsp)[isNull]) {
			gam_preds[[i]]<-rep(mean(DD_t[[j]][[i]][,1]),times=nsites)
			}
		gam_preds<-simplify2array(gam_preds)

		gam_PAs <- array(NA, dim=list(nsites, nsp, REPs))
		for (n in 1:REPs) {
			gam_PAs[,!isNull,n] <- (gam_preds[,!isNull]+unlist(rnorm(gam_preds[,!isNull],mean=0,sd=1))>0)*1
			gam_PAs[,isNull,n] <- rbinom(gam_preds[,isNull],1,gam_preds[,isNull])
			}
		rm(gams)
		rm(gam_preds)

		if (m==1) { 
			save(gam_PAs, file=paste(PD2,set_no,"/gam_PAs_",j,"_",dataN[sz],".RData",sep=""))
			}
		if (m==2) { 
			gam2_PAs<-gam_PAs
			save(gam2_PAs, file=paste(PD2,set_no,"/gam2_PAs_",j,"_",dataN[sz],".RData",sep=""))
			}
		if (m==3) { 
			gam_spat1_PAs<-gam_PAs
			save(gam_spat1_PAs, file=paste(PD2,set_no,"/gam_spat1_PAs_",j,"_",dataN[sz],".RData",sep=""))
			}
		if (m==4) {
			gam_spat2_PAs<-gam_PAs
			save(gam_spat2_PAs, file=paste(PD2,set_no,"/gam_spat2_PAs_",j,"_",dataN[sz],".RData",sep=""))
			}

		rm(gam_PAs)
		gc()

		}
	}	

##########################################################################################
