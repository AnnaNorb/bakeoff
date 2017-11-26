##########################################################################################
# MULTIVARIATE ADAPTIVE REGRESSION SPLINES
##########################################################################################

require(earth)

##########################################################################################

for (j in 1:3) {

	if (j==1) { sT<-Sys.time() }
		mars	<-	earth(x=x_train[[j]][,-1], y=y_train[[j]],
			        degree=1, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars, file=paste(FD,set_no,"/mars_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_COMM_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
		mars_int	<-	earth(x=x_train[[j]][,-1], y=y_train[[j]],
                    degree=2, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars_int, file=paste(FD,set_no,"/mars_int_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_INT_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
		mars2	<-	earth(x=cbind(x_train[[j]][,-1],s_train[[j]]), y=y_train[[j]],
			        degree=1, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars2, file=paste(FD,set_no,"/mars2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_COMM2_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
		mars_int2	<-	earth(x=cbind(x_train[[j]][,-1],s_train[[j]]), y=y_train[[j]],
                    degree=2, glm=list(family=binomial(link="probit")))
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mars_int2, file=paste(FD,set_no,"/mars_int2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MARS_INT2_",dataN[sz],".RData",sep=""))
	}

}

##########################################################################################
