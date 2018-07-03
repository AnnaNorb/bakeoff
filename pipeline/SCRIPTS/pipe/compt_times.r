##########################################################################################
# COMPUTATION TIMES
##########################################################################################

# compile
##########################################################################################
rm(list=ls()[!(ls() %in% c("OS","pth","SETT"))]); gc(); source(SETT); setwd(WD)
require(rmatio)
require(R.matlab)

for (sz in 1:2) {
	for (d in 1:length(Sets)) {
		set_no <- Sets[d]
		comtimes<-list()
		filebody <- file.path(FD,set_no)
		for (m in 1:length(mod_names3)) {
			if (m <= 20) {
				fileNam <- paste(filebody,
								 "/comTimes_",
								 mod_names3[m],"_",
								 dataN[sz],".RData",sep="")
				load(file=fileNam)
				comtimes[[m]]<-as.numeric(comTimes, units = "mins")	
			} 
			if (m > 20 & m <= 22) {
				fileNam <- paste(filebody,
								 "/ssHMSC/compTime_",
								 Sets[d],"_",
								 mod_names3[m],"_",
								 dataN[sz],".mat",sep="")
				tmp<-read.mat(filename=fileNam)
				comtimes[[m]]<-as.numeric(tmp)/60
			}	
			if (m > 22) {
				fileNam <- paste(filebody,
								 "/compTime_hmsc_",
								 Sets[d],"_",m-22,"_",
								 dataN[sz],".mat",sep="")
				tmp<-read.mat(filename=fileNam)
				comtimes[[m]]<-as.numeric(tmp)/60
			}	
		}
		save(comtimes, file=paste(RD2,set_no,"/comtimes_",dataN[sz],".RData",sep=""))
	}
}

# plot computation times
##########################################################################################
rm(list=ls()[!(ls() %in% c("OS","pth","SETT"))]); gc(); source(SETT); setwd(WD)

# define the ensemble
ens<-c("HMSC3", "BC2", "BRT1", "MISTN1", "GNN1")

ComTimesAll<-NA
for (sz in 1:2) {
	set_no <- Sets[1]
	load(file=paste(RD2,set_no,"/comtimes_",dataN[sz],".RData",sep=""))
	ComTimes<-unlist(comtimes)
	for (d in 2:length(Sets)) {
		set_no <- Sets[d]
		load(file=paste(RD2,set_no,"/comtimes_",dataN[sz],".RData",sep=""))
		ComTimes<-cbind(ComTimes,unlist(comtimes))
	}
	ComTimes<-cbind(mod_names,ComTimes,rep(dataN[sz],times=nrow(ComTimes)))
	colnames(ComTimes)<-c("Variant",Sets,"dataSize")
	ComTimesAll<-rbind(ComTimesAll,ComTimes)
}
ComTimesAll<-ComTimesAll[-1,]

comtimes<-ComTimesAll[,2:7]
comtimes<-matrix(unlist(comtimes),ncol=6)
rownames(comtimes)<-ComTimesAll[,1]
colnames(comtimes)<-c(Sets,"size")
modNams<-rownames(comtimes[comtimes[,ncol(comtimes)]=="150",])
comtimes150<-apply(comtimes[comtimes[,ncol(comtimes)]=="150",],2,as.numeric)
comtimes300<-apply(comtimes[comtimes[,ncol(comtimes)]=="300",],2,as.numeric)

# ensembles
comtimes150 <- rbind(comtimes150, c(colSums(comtimes150[,-ncol(comtimes150)]),150))
rownames(comtimes150)<-c(modNams,"ENS_all")
comtimes300 <- rbind(comtimes300, c(colSums(comtimes300[,-ncol(comtimes300)]),300))
rownames(comtimes300)<-c(modNams,"ENS_all")

comtimes150 <- rbind(comtimes150,c(colSums(comtimes150[ens,-ncol(comtimes150)]),150))
rownames(comtimes150)[nrow(comtimes150)]<-paste(c("ENS",ens),collapse="_")
comtimes300 <- rbind(comtimes300,c(colSums(comtimes300[ens,-ncol(comtimes300)]),300))
rownames(comtimes300)[nrow(comtimes300)]<-paste(c("ENS",ens),collapse="_")
comtimes<-rbind(comtimes150,comtimes300)
comtimes<-cbind(rownames(comtimes),comtimes)
write.table(comtimes,file=paste(RDfinal,"comtimes.csv",sep=""),row.names=FALSE,col.names=TRUE,sep=",", quote=FALSE)

comTimes<-apply(comtimes[,-1],2,as.numeric)
rownames(comTimes)<-rownames(comtimes)

pdf(file=paste(RDfinal,"/comptimes.pdf",sep=""),bg="transparent",width=10,height=7)
	par(family="serif",mar=c(18,4,4,1))
	plot(log10(apply(comTimes[which(comTimes[,ncol(comTimes)]==300),-ncol(comTimes)],1,max)),col="black",pch=17,
		xlab="",ylab="",yaxt="n",xaxt="n",ylim=c(log10(0.0004),log10(4000)), main="Computation times")
	points(log10(apply(comTimes[which(comTimes[,ncol(comTimes)]==150),-ncol(comTimes)],1,min)))
	axis(side=2,at=log10(c(0.0005,0.0167,1,10,60,1440,3900)),labels=c("0.03 sec","1 sec","1 min","10 min","1 h","24 h","65 h"),las=2)
	axis(side=1,at=1:nrow(comTimes[which(comTimes[,ncol(comTimes)]==150),]),labels=rownames(comTimes[which(comTimes[,ncol(comTimes)]==150),]),las=2)
	for (i in 1:nrow(comTimes[which(comTimes[,ncol(comTimes)]==300),-ncol(comTimes)])) {
		lines(y=c(log10(apply(comTimes[which(comTimes[,ncol(comTimes)]==150),-ncol(comTimes)],1,min))[i],
					log10(apply(comTimes[which(comTimes[,ncol(comTimes)]==300),-ncol(comTimes)],1,max))[i]),
					x=c(i,i))
	}
	abline(h=log10(1440))
dev.off()

##########################################################################################
