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
	brt <- list()
	brt2 <- list()

	if (j==1) { sT<-Sys.time() }
	
	gbmNtrees<-matrix(NA,nrow=length(DD_t[[j]]))
	
	for (i in 1:length(DD_t[[j]])) {

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
					brt[[i]] <- NA
				} else {
					brt[[i]] <- gbms[[which(gbmErr==min(gbmErr,na.rm=T),arr.ind=T)[1]]]
					gbmNtrees[i,] <- brt[[i]]$n.trees
				}

			rm(gbms)
			rm(gbmErr)
			gc()
	}
		if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
		
save(brt, file=paste(FD,set_no,"/brt_",j,"_",dataN[sz],".RData",sep=""))
save(gbmNtrees, file=paste(FD,set_no,"/gbmNtrees",j,"_",dataN[sz],".RData",sep=""))
if (j==1) {
save(comTimes, file=paste(FD,set_no,"/comTimes_BRT_",dataN[sz],".RData",sep=""))
}

rm(brt)
gc()

	if (j==1) { sT<-Sys.time() }
	
	gbmNtrees2<-matrix(NA,nrow=length(DD_t[[j]]))

	for (i in 1:length(DD_t[[j]])) {

		gbms2<-list()
		gbmErr2<-matrix(NA,nrow=nrow(vals))
  
			for (m in 1:nrow(vals)){

    			tryCatch({
				gbmod	<- gbm.step(data=DD_t[[j]][[i]], gbm.x = c(3:ncol(DD_t[[j]][[i]])), 
							gbm.y = 1, prev.stratify = TRUE,
                			family = "bernoulli", plot.main=FALSE, plot.folds=FALSE, 
            				#n.trees=vals[m,3], 
                			learning.rate=vals[m,2], tree.complexity=vals[m,1])

				if (is.null(gbmod)==FALSE){
					gbms2[[m]] <- gbmod
					gbmErr2[m,] <- gbmod$cv.statistics$deviance.mean
				}
    			}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
			}

				if (all(is.na(gbmErr2))){
					brt2[[i]] <- NA
				} else {
					brt2[[i]] <- gbms2[[which(gbmErr2==min(gbmErr2,na.rm=T),arr.ind=T)[1]]]
					gbmNtrees2[i,] <- brt2[[i]]$n.trees
				}

			rm(gbms2)
			rm(gbmErr2)
			gc()
	}
		if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

save(brt2, file=paste(FD,set_no,"/brt2_",j,"_",dataN[sz],".RData",sep=""))
save(gbmNtrees2, file=paste(FD,set_no,"/gbmNtrees2",j,"_",dataN[sz],".RData",sep=""))
if (j==1) {
save(comTimes, file=paste(FD,set_no,"/comTimes_BRT2_",dataN[sz],".RData",sep=""))
}

rm(brt2)
gc()

}

##########################################################################################
