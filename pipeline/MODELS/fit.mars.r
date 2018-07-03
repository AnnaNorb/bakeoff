##########################################################################################
# MULTIVARIATE ADAPTIVE REGRESSION SPLINES
##########################################################################################

require(earth)

##########################################################################################

for (j in 1:3) {

	ncovar<-(ncol(x_train[[1]])-1)/2
	Xt<-x_train[[j]][,c(2:(ncovar+1))]
	
	if (j==1) { sT<-Sys.time() }
		mars1	<-	earth(x=Xt, y=y_train[[j]],
			        degree=1, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars1, file=file.path(FD,set_no,paste("mars1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_MARS1_",dataN[sz],".RData",sep="")))
		rm(comTimes)
	}

	if (j==1) { sT<-Sys.time() }
		mars2	<-	earth(x=Xt, y=y_train[[j]],
                    	degree=2, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars2, file=file.path(FD,set_no,paste("mars2_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_MARS2_",dataN[sz],".RData",sep="")))
		rm(comTimes)
	}
	
	rm(mars1)
	rm(mars2)
	gc()
}

##########################################################################################
