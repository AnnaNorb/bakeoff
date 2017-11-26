##########################################################################################
# RAUPR-CRICK-DISSIMILARITY BETWEEN SPECIES (NOT SITES)
##########################################################################################

require(vegan)

spRAUP <- function(Y) { 
	res<-matrix(raupcrick(t(Y)),ncol=1)
	return(res)
	}

##########################################################################################

RaupC <- array(NA,dim=list(nmodels,choose(ncol(y_valid[[1]]),2),3))
RaupC_MSEs <- array(NA,dim=list(nmodels,choose(ncol(y_valid[[1]]),2),3))

for (j in 1:3) {

# true dissimilarities (default raupcrick)
trueRaupC<-spRAUP(y_valid[[j]])

	for (m in models) {

    	model <- local({
    	load(paste(PD2, Sets[d],"/",pred_names[[m]],j,"_",dataN[sz],".RData",sep=""))
    	stopifnot(length(ls())==1)
    	environment()[[ls()]]
    	})

	pred_comms <- simplify2array(model)
	pred_comms <- as.array(pred_comms)

	if (any(is(pred_comms)== "simple_sparse_array")) {
		pred_comms<- array(pred_comms$v, dim=pred_comms$dim)
	}

	# calculating dissimilarities
	raupc<-apply(pred_comms,3,spRAUP)
	RaupC[m,,j]<-rowMeans(raupc)

	Tmp<-raupc[,1]-trueRaupC

	for (i in 2:REPs) {
		tmp<-raupc[,i]-trueRaupC
		Tmp<-Tmp+tmp
		}
		
	RaupC_MSEs[m,,j]<-(Tmp/REPs)^2

	}
}

save(RaupC, file=paste(RD2, 'coocMsrs/', set_no, "/RaupC.r", sep=""))
save(RaupC_MSEs, file=paste(RD2, 'coocMsrs/', set_no, "/RaupC_MSEs.r", sep=""))

##########################################################################################
