##########################################################################################
# STATISTICAL METHODS FOR ANALYSING MULTIVARIATE ABUNDANCE DATA
##########################################################################################

require(mvabund)

##########################################################################################

for (j in 1:3) {

	Yt <- mvabund(y_train[[j]])
	Xt <- data.frame(x_train[[j]][,-1])
	Xts <- data.frame(cbind(x_train[[j]][,-1],s_train[[j]]))

	if (j==1) { sT<-Sys.time() }
	traitglm1 <- traitglm(L=Yt, R=Xt, family="binomial(link=logit)", method='glm1path')
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(traitglm1, file=paste(FD,set_no,"/traitglm1_",j,"_",dataN[sz],".RData",sep=""))
	save(comTimes, file=paste(FD,set_no,"/comTimes_MVABUND1_",dataN[sz],".RData",sep=""))

	if (j==1) { sT<-Sys.time() }
	traitglm2 <- traitglm(L=Yt, R=Xts, family="binomial(link=logit)", method='glm1path')
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(traitglm2, file=paste(FD,set_no,"/traitglm2_",j,"_",dataN[sz],".RData",sep=""))
	save(comTimes, file=paste(FD,set_no,"/comTimes_MVABUND2_",dataN[sz],".RData",sep=""))

	if (j==1) { sT<-Sys.time() }
	traitglm3 <- traitglm(L=Yt, R=Xt, family="binomial(link=logit)", method='glm1path', composition=TRUE)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(traitglm3, file=paste(FD,set_no,"/traitglm3_",j,"_",dataN[sz],".RData",sep=""))
	save(comTimes, file=paste(FD,set_no,"/comTimes_MVABUND3_",dataN[sz],".RData",sep=""))

	rm(Yt)
	rm(Xt)
	rm(traitglm)
	gc()

}
##########################################################################################
