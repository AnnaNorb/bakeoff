##########################################################################################
# STATISTICAL METHODS FOR ANALYSING MULTIVARIATE ABUNDANCE DATA
##########################################################################################

require(mvabund)

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(Yt~V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
	}
if (set_no=="trees") { 
	form <- as.formula(Yt~V1+V2+V3+V1_2+V2_2+V3_2)
	}
if (set_no=="vegetation") { 
	form <- as.formula(Yt~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
	}

##########################################################################################

for (j in 1:3) {

	Yt <- mvabund(y_train[[j]])
	Xt <- data.frame(x_train[[j]][,-1])

	if (j==1) { sT<-Sys.time() }
	manyglm1 <- manyglm(form, data=Xt, family="binomial(link=logit)")
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		save(comTimes, file=paste(FD,set_no,"/comTimes_MVABUND1_",dataN[sz],".RData",sep=""))
	}
	save(manyglm1, file=paste(FD,set_no,"/manyglm1_",j,"_",dataN[sz],".RData",sep=""))

	rm(Yt)
	rm(Xt)
	rm(manyglm1)
	if (j==1) { rm(comTimes) }
	gc()

}
##########################################################################################
