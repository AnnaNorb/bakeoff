##########################################################################################
# MULTIVARIATE ADAPTIVE REGRESSION SPLINES
##########################################################################################

require(earth)

##########################################################################################

for (j in 1:3) {

	ncovar<-(ncol(x_train[[1]])-1)/2
	Xt<-x_train[[j]][,c(2:(ncovar+1))]
	
	if (j==1) { sT<-Sys.time() }
		mars	<-	earth(x=Xt, y=y_train[[j]],
			        degree=1, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars, file=paste(FD,set_no,"/mars_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_COMM1_",dataN[sz],".RData",sep=""))
		rm(comTimes)
	}

	if (j==1) { sT<-Sys.time() }
		mars_int	<-	earth(x=Xt, y=y_train[[j]],
                    	degree=2, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars_int, file=paste(FD,set_no,"/mars_int_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_INT1_",dataN[sz],".RData",sep=""))
		rm(comTimes)
	}
	
	rm(mars)
	rm(mars_int)
	gc()
}

##########################################################################################
