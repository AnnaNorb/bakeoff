##########################################################################################
# BOOSTED REGRESSION TREES
##########################################################################################

require("gbm")
require("dismo")

tcs<-c(2:5)
lrs<-c(0.1,0.01,0.001,0.0001)
vals<-cbind(rep(tcs,each=length(lrs)),rep(lrs,times=length(tcs)))

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	ncovar<-(ncol(x_train[[1]])-1)/2
	brt1 <- list()
		
	if (j==1) { sT<-Sys.time() }
	
	gbmNtrees<-matrix(NA,nrow=length(DD_t[[j]]))
		
	for (i in 1:nsp) {

		gbms<-list()
		gbmErr<-matrix(NA,nrow=nrow(vals))

		for (m in 1:nrow(vals)){
			tryCatch({
			gbmod	<- gbm.step(data=DD_t[[j]][[i]], gbm.x = c(3:(ncovar+2)), gbm.y = 1, prev.stratify = TRUE,
						family = "bernoulli", plot.main=FALSE, plot.folds=FALSE, 
						#n.trees=vals[m,3], 
						learning.rate=vals[m,2], tree.complexity=vals[m,1])

				if (is.null(gbmod)==FALSE){
					gbms[[m]] <- gbmod
					gbmErr[m,] <- gbmod$cv.statistics$deviance.mean
				}
			}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		}

		if (all(is.na(gbmErr))){
			brt1[[i]] <- NA
		} else {
			brt1[[i]] <- gbms[[which(gbmErr==min(gbmErr,na.rm=T),arr.ind=T)[1]]]
		}

		rm(gbms)
		rm(gbmErr)
		gc()
	}
 
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
		
	save(brt1, file=file.path(FD,set_no,paste("brt1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_BRT1_",dataN[sz],".RData",sep="")))
	}

	rm(brt1)
	if (j==1) { rm(comTimes) }
	gc()
}


##########################################################################################
