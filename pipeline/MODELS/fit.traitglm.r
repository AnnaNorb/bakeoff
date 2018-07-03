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
	#Xts <- data.frame(cbind(x_train[[j]][,-1],s_train[[j]]))

	if (j==1) { sT<-Sys.time() }
	traitglm1 <- traitglm(L=Yt, R=Xt, family="binomial", method='glm1path')
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(traitglm1, file=file.path(FD,set_no,paste("traitglm1_",j,"_",dataN[sz],".RData",sep="")))

	if (j==1) { 
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_TRAITGLM1_",dataN[sz],".RData",sep=""))) 
		rm(comTimes)
	}
		
# 	if (j==1) { sT<-Sys.time() }
# 	traitglm3 <- traitglm(L=Yt, R=Xt, family="binomial", method='glm1path', composition=TRUE)
# 	if (j==1) {
# 		eT<-Sys.time()
# 		comTimes<-eT-sT
# 		}
# 	save(traitglm3, file=paste(FD,set_no,"/traitglm3_",j,"_",dataN[sz],".RData",sep=""))
# 	save(comTimes, file=paste(FD,set_no,"/comTimes_MVABUND3_",dataN[sz],".RData",sep=""))

	rm(Yt)
	rm(Xt)
	rm(traitglm1)
	gc()

}
##########################################################################################
