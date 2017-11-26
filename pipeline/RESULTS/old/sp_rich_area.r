##########################################################################################
# SP ~ AREA - REAL DATA	
##########################################################################################

#Ennustetaan lajien runsaus (hierarkisissa datoissa per site, spatiaalisissa esim. aggregoidaan solut spatiaalisesti 10 solun ryppäiksi, olkoon se nimeltään "site"), 
#ja verrataan (lajikohtaisesti) true abundance vs. predicted abundance, 
#tai true presence-absence vs. predicted presence absence, per site. 
#Tai sitten verrataan species richness (expected / bias in 50-95%) per site. 
#Tai sitten konstruoidaan koko species-area relationship ja verrataan esim. sen expontenttia z. 
#Ehkä simppeleintä (koodaamisen ja selittämisen suhteen) olisi aggregoida ne ennusteet to presence-absence per site, 
#ja sitten laskea Tjur R^2, deviance, ja species richness mitat samalla tavalla kuin ne on laskettu per sampling unit. 

sprichAreaCI50_I <- matrix(NA, nrow=nmodels, ncol=length(Sets))
sprichAreaCI50_E <- matrix(NA, nrow=nmodels, ncol=length(Sets))

sprichAreaCI95_I <- matrix(NA, nrow=nmodels, ncol=length(Sets))
sprichAreaCI95_E <- matrix(NA, nrow=nmodels, ncol=length(Sets))

sprichArea_err2means_I <- matrix(NA, nrow=nmodels, ncol=length(Sets))
sprichArea_err2means_E <- matrix(NA, nrow=nmodels, ncol=length(Sets))

sprichAreaCI50err_i<-matrix(NA,nrow=nmodels,ncol=length(Sets))
sprichAreaCI50err_e<-matrix(NA,nrow=nmodels,ncol=length(Sets))
sprichAreaCI95err_i<-matrix(NA,nrow=nmodels,ncol=length(Sets))
sprichAreaCI95err_e<-matrix(NA,nrow=nmodels,ncol=length(Sets))

RealSprichArea50CIs_I<-list()
RealSprichArea50CIs_E<-list()
RealSprichArea95CIs_I<-list()
RealSprichArea95CIs_E<-list()

for (d in 1:length(Sets)) {

##########################################################################################

set_no <- Sets[d]
source(paste(WD,"read.format.bakeoff.real.data.r",sep=""))

nsites_i<-nrow(X_I_valid)
nsites_e<-nrow(X_E_valid)

# spat
##########################################################################################
if (set_no=="butterfly"|set_no=="bird"|set_no=="plant"|set_no=="tree") {

clusts_i<-kmeans(S_I_valid, round(nsites_i/10))$cluster
clusts_e<-kmeans(S_E_valid, round(nsites_e/10))$cluster
svalid_i<-cbind(S_I_valid, clusts_i)
svalid_e<-cbind(S_E_valid,  clusts_e)
nareas_i<-length(unique(clusts_i))
nareas_e<-length(unique(clusts_e))

realSprichArea50CIs_i<-matrix(NA,nrow=nareas_i,ncol=nmodels)
realSprichArea50CIs_e<-matrix(NA,nrow=nareas_e,ncol=nmodels)
realSprichArea95CIs_i<-matrix(NA,nrow=nareas_i,ncol=nmodels)
realSprichArea95CIs_e<-matrix(NA,nrow=nareas_e,ncol=nmodels)

# hierarchical
##########################################################################################
}else{
nareas_i<-length(unique(S_I_valid))
nareas_e<-length(unique(S_E_valid))
realSprichArea50CIs_i<-matrix(NA,nrow=nareas_i,ncol=nmodels)
realSprichArea50CIs_e<-matrix(NA,nrow=nareas_e,ncol=nmodels)
realSprichArea95CIs_i<-matrix(NA,nrow=nareas_i,ncol=nmodels)
realSprichArea95CIs_e<-matrix(NA,nrow=nareas_e,ncol=nmodels)
}

realSprichArea50CIs_i<-matrix(NA,ncol=nmodels,nrow=nareas_i)
realSprichArea95CIs_i<-matrix(NA,ncol=nmodels,nrow=nareas_i)
realSprichArea50CIs_e<-matrix(NA,ncol=nmodels,nrow=nareas_e)
realSprichArea95CIs_e<-matrix(NA,ncol=nmodels,nrow=nareas_e)

##########################################################################################

for (m in 1:nmodels) {
    setwd(paste(PD2, "data", Sets[d], sep=""))
    model <- local({
    load(preds_I[[m]])
    stopifnot(length(ls())==1)
    environment()[[ls()]]
    })
    model_e <- local({
    load(preds_E[[m]])
    stopifnot(length(ls())==1)
    environment()[[ls()]]
    })

	pred_comms_I <- simplify2array(model)
	pred_comms_E <- simplify2array(model_e)
    pred_comms_I <- as.array(pred_comms_I)
	pred_comms_E <- as.array(pred_comms_E)

	if (is(pred_comms_E)== "simple_sparse_array" | is(pred_comms_E)== "simple_sparse_array") {
		pred_comms_I<- array(pred_comms_I$v, dim=pred_comms_I$dim)
		pred_comms_E<- array(pred_comms_E$v, dim=pred_comms_E$dim)
	}
    
nsamp <- dim(pred_comms_I)[3]

# spat
##########################################################################################
if (set_no=="butterfly"|set_no=="bird"|set_no=="plant"|set_no=="tree") {

sprich_i<-matrix(NA,ncol=nareas_i,nrow=nsamp)
realSprichArea_i<-matrix(NA,ncol=nareas_i)
for (s in 1:nareas_i) {
ss<-as.numeric(unique(svalid_i[,ncol(svalid_i)]))[s]
Sel<-which(svalid_i[,ncol(svalid_i)]==ss,arr.ind=T)
	if (length(Sel)==1){
	sprich_i[,s]<-colSums((pred_comms_I[Sel,,])>0)
	realSprichArea_i[,s]<-sum(y_I_valid[Sel,]>0)
	}else{
	sprich_i[,s]<-colSums(apply(pred_comms_I[Sel,,],3,colSums)>0)
	realSprichArea_i[,s]<-sum(colSums(y_I_valid[Sel,])>0)
	}
}

sprich_e<-matrix(NA,ncol=nareas_e,nrow=nsamp)
realSprichArea_e<-matrix(NA,ncol=nareas_e)
for (s in 1:nareas_e) {
ss<-as.numeric(unique(svalid_e[,ncol(svalid_e)]))[s]
Sel<-which(svalid_e[,ncol(svalid_e)]==ss,arr.ind=T)
	if (length(Sel)==1){
	sprich_e[,s]<-colSums((pred_comms_E[Sel,,])>0)
	realSprichArea_e[,s]<-sum(y_E_valid[Sel,]>0)
	}else{
	sprich_e[,s]<-colSums(apply(pred_comms_E[Sel,,],3,colSums)>0)
	realSprichArea_e[,s]<-sum(colSums(y_E_valid[Sel,])>0)
	}
}

# hirarchical
##########################################################################################
}else{

sprich_i<-matrix(NA,ncol=nareas_i,nrow=nsamp)
realSprichArea_i<-matrix(NA,ncol=nareas_i)
for (s in 1:nareas_i) {
ss<-as.numeric(unique(svalid_i))[s]
Sel<-which(svalid_i==ss,arr.ind=T)[,1]
	if (length(Sel)==1){
	sprich_i[,s]<-colSums((pred_comms_I[Sel,,])>0)
	realSprichArea_i[,s]<-sum(y_I_valid[Sel,]>0)
	}else{
	sprich_i[,s]<-colSums(apply(pred_comms_I[Sel,,],3,colSums)>0)
	realSprichArea_i[,s]<-sum(colSums(y_I_valid[Sel,])>0)
	}
}

sprich_e<-matrix(NA,ncol=nareas_e,nrow=nsamp)
realSprichArea_e<-matrix(NA,ncol=nareas_e)
for (s in 1:nareas_e) {
ss<-as.numeric(unique(svalid_e))[s]
Sel<-which(svalid_e==ss,arr.ind=T)[,1]
	if (length(Sel)==1){
	sprich_e[,s]<-colSums((pred_comms_E[Sel,,])>0)
	realSprichArea_e[,s]<-sum(y_E_valid[Sel,]>0)
	}else{
	sprich_e[,s]<-colSums(apply(pred_comms_E[Sel,,],3,colSums)>0)
	realSprichArea_e[,s]<-sum(colSums(y_E_valid[Sel,])>0)
	}
}
}

# CIs
##########################################################################################

# 0.25% and 0.75 quantiles of predicted species richnesses
sprich50q_i <- t(apply(sprich_i, 2, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
sprich50q_e <- t(apply(sprich_e, 2, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
# 0.05% and 0.95 quantiles of predicted species richnesses
sprich95q_i <- t(apply(sprich_i, 2, quantile, probs=c(0.025,0.975), na.rm=T, type=3))
sprich95q_e <- t(apply(sprich_e, 2, quantile, probs=c(0.025,0.975), na.rm=T, type=3))

for (j in 1:dim(sprich_i)[2]){
realSprichArea50CIs_i[j,m]<-sum(sprich_i[,j]>=sprich50q_i[j,1] & sprich_i[,j]<=sprich50q_i[j,2])/nsamp
realSprichArea95CIs_i[j,m]<-sum(sprich_i[,j]>=sprich95q_i[j,1] & sprich_i[,j]<=sprich95q_i[j,2])/nsamp
}
for (j in 1:dim(sprich_e)[2]){
realSprichArea50CIs_e[j,m]<-sum(sprich_e[,j]>=sprich50q_e[j,1] & sprich_e[,j]<=sprich50q_e[j,2])/nsamp
realSprichArea95CIs_e[j,m]<-sum(sprich_e[,j]>=sprich95q_e[j,1] & sprich_e[,j]<=sprich95q_e[j,2])/nsamp
}

# no. cases within the 25% and 75% quantiles = 50% CI    
sprich_50ci_i <- (sum(realSprichArea_i>=sprich50q_i[,1]&realSprichArea_i<=sprich50q_i[,2]))/nareas_i
sprich_50ci_e <- (sum(realSprichArea_e>=sprich50q_e[,1]&realSprichArea_e<=sprich50q_e[,2]))/nareas_e

sprichAreaCI50_I[m,d] <- sprich_50ci_i
sprichAreaCI50_E[m,d] <- sprich_50ci_e

# no. cases within the 5% and 95% quantiles = 90% CI        
sprich_95ci_i <- (sum(realSprichArea_i>=sprich95q_i[,1]&realSprichArea_i<=sprich95q_i[,2]))/nareas_i
sprich_95ci_e <- (sum(realSprichArea_e>=sprich95q_e[,1]&realSprichArea_e<=sprich95q_e[,2]))/nareas_e

sprichAreaCI95_I[m,d] <- sprich_95ci_i
sprichAreaCI95_E[m,d] <- sprich_95ci_e

# errors in CIs
sprichAreaCI50err_i[m,d]<-sprichAreaCI50_I[m,d]-mean(realSprichArea50CIs_i[,m])
sprichAreaCI50err_e[m,d]<-sprichAreaCI50_E[m,d]-mean(realSprichArea50CIs_e[,m])
sprichAreaCI95err_i[m,d]<-sprichAreaCI95_I[m,d]-mean(realSprichArea95CIs_i[,m])
sprichAreaCI95err_e[m,d]<-sprichAreaCI95_E[m,d]-mean(realSprichArea95CIs_e[,m])
  
# MEANS AND ERRORS
##########################################################################################

# means squared errors over the sites
sprichArea_err2means_I[m,d] <- mean((colMeans(sprich_i, na.rm=T)-realSprichArea_i)^2)
sprichArea_err2means_E[m,d] <- mean((colMeans(sprich_e, na.rm=T)-realSprichArea_e)^2)
 
}
RealSprichArea50CIs_I[[d]]<-realSprichArea50CIs_i
RealSprichArea50CIs_E[[d]]<-realSprichArea50CIs_e
RealSprichArea95CIs_I[[d]]<-realSprichArea95CIs_i
RealSprichArea95CIs_E[[d]]<-realSprichArea95CIs_e
}

save(sprichArea_err2means_I, file=paste(RDfinal,"sprichArea_err2means_I.r",sep=""))
save(sprichArea_err2means_E, file=paste(RDfinal,"sprichArea_err2means_E.r",sep=""))

save(sprichAreaCI50_I, file=paste(RDfinal,"sprichAreaCI50_I.r",sep=""))
save(sprichAreaCI50_E, file=paste(RDfinal,"sprichAreaCI50_E.r",sep=""))

save(sprichAreaCI95_I, file=paste(RDfinal,"sprichAreaCI95_I.r",sep=""))
save(sprichAreaCI95_E, file=paste(RDfinal,"sprichAreaCI95_E.r",sep=""))

save(RealSprichArea50CIs_I, file=paste(RDfinal,"RealSprichArea50CIs_I.r",sep=""))
save(RealSprichArea50CIs_E, file=paste(RDfinal,"RealSprichArea50CIs_E.r",sep=""))
save(RealSprichArea95CIs_I, file=paste(RDfinal,"RealSprichArea95CIs_I.r",sep=""))
save(RealSprichArea95CIs_E, file=paste(RDfinal,"RealSprichArea95CIs_E.r",sep=""))

save(sprichAreaCI50err_i, file=paste(RDfinal,"sprichAreaCI50err_i.r",sep=""))
save(sprichAreaCI50err_e, file=paste(RDfinal,"sprichAreaCI50err_e.r",sep=""))
save(sprichAreaCI95err_i, file=paste(RDfinal,"sprichAreaCI95err_i.r",sep=""))
save(sprichAreaCI95err_e, file=paste(RDfinal,"sprichAreaCI95err_e.r",sep=""))

##########################################################################################

