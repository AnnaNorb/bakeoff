##########################################################################################
# RAW RESULTS FOR PERFORMANCE MEASURES
##########################################################################################
rm(list=ls()[!(ls() %in% c("OS","pth","SETT","ENS"))]); gc(); source(SETT); setwd(WD)

load(file=file.path(RDfinal,"TBL_PMS_ALL.RData"))
resTBL<-data.frame(TBL_PMS_ALL)
resTBL[,c(3:9,12:(ncol(resTBL)-1))]<-apply(resTBL[,c(3:9,12:(ncol(resTBL)-1))],2,as.numeric)
pms<-colnames(resTBL[,12:(ncol(resTBL)-1)])
pms<-pms[c(1,2,4,3,
		5,9,17,13,
		6,10,18,14,
		7,11,19,15,
		8,12,20,16)]
resTBL2<-resTBL
resTBL[,minTomaxBest] <- -resTBL[,minTomaxBest]

# plot
##########################################################################################
pdf(file=file.path(RDfinal,"raw_res_fig.pdf"),bg="transparent",width=15,height=15)
	par(family="serif",mfrow=c(5,4),mar=c(7,3,2,1))
	for (p in 1:length(pms)) {
		plot(0,0,
			 xlim=c(0,length(mod_names2)),
			 ylim=c(min(resTBL[,pms[p]]),max(resTBL[,pms[p]])),
			 type='n',
			 xaxt='n',
			 xlab="",ylab="",
			 main=pms[p])
		for (m in 1:length(mod_names)) {
			lines(resTBL[which(resTBL$Abbreviation==mod_names[[m]]),pms[p]],
				  x=rep(m,times=length(resTBL[which(resTBL$Abbreviation==mod_names[[m]]),pms[p]])),
				  lwd=2)
			points(mean(resTBL[which(resTBL$Abbreviation==mod_names[[m]]),pms[p]]),
				   x=m,pch=21,col="black",bg="red3",cex=2)
		}
		axis(1,1:length(mod_names),unlist(mod_names),las=2)
	}
dev.off()

##########################################################################################
