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

spRichAreaErr2means <- matrix(NA, nrow=nmodels, ncol=3)

for (j in 1:3) {		
	
	nsites <- nrow(s_valid[[j]])
	nsp <- ncol(y_valid[[j]])
	clusts <- kmeans(s_valid[[j]], round(nsites/10))$cluster
	svalid <- cbind(s_valid[[j]], clusts)
	nareas <- length(unique(clusts))

	for (m in 1:nmodels) {

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

		sprich <- matrix(NA,ncol=nareas,nrow=REPs)
		realSprichArea <- matrix(NA,ncol=nareas)

		for (s in 1:nareas) {
			ss<-as.numeric(unique(svalid[,ncol(svalid)]))[s]
			Sel<-which(svalid[,ncol(svalid)]==ss,arr.ind=T)
			if (length(Sel)==1){
				sprich[,s]<-colSums((pred_comms[Sel,,])>0)
				realSprichArea[,s]<-sum(y_valid[[j]][Sel,]>0)
			}else{
				sprich[,s]<-colSums(apply(pred_comms[Sel,,],3,colSums)>0)
				realSprichArea[,s]<-sum(colSums(y_valid[[j]][Sel,])>0)
				}
			}

		# means squared errors over the sites
		spRichAreaErr2means[m,j] <- mean((colMeans(sprich, na.rm=T)-realSprichArea)^2)
 
		}
	}

save(spRichAreaErr2means, file=paste(RDfinal,dataN[sz],"/spRichAreaErr2means_",Sets[d],".RData",sep=""))

##########################################################################################
