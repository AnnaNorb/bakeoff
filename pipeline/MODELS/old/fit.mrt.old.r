# analysis with Multivariate regression tree
##########################################################################################
#install.packages(paste(WD,"MODELS/mrts/mvpart_1.6-2.tar",sep=''), repos = NULL, type="source")
library('mvpart')
library('caret')

ntrees<-seq(10,1000,90)

# Interpolation
##########################################################################################

dataAll<-as.data.frame(cbind(y_I_train,X_I_train))
colnames(dataAll)<-paste("V",1:ncol(dataAll),sep="")
nsp <- ncol(y_I_train)
nsites <- nrow(dataAll)

mrt_I<-list()
mrt_I_spat<-list()
mrt_i<-list()
mrt_i_spat<-list()
mrt_i_spat1<-list()
mrt_i_spat2<-list()
mrtAccuracy_I<-matrix(NA, nrow=length(ntrees),ncol=4)
dada_I<-unlist(dataAll[,1:ncol(y_I_train)])


sT<-Sys.time()
for (m in 1:length(ntrees)){
mrt_i[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_I_train)])~., data=as.data.frame(dataAll[,(ncol(y_I_train)+2):(ncol(dataAll))]),
              size=ntrees[m],xv="lse",xval=10,xvmult=0,plot.add=FALSE,control=list(cp=0.001))

pred<-predict(mrt_i[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_I[m,1]<-confusionMatrix(data=pred,reference=dada_I)$overall[1]
}
eT<-Sys.time()
comTimes$MRTS<-eT-sT

sT<-Sys.time()
for (m in 1:length(ntrees)){
	if (ncol(S_I_train)==1) {
mrt_i_spat[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_I_train)])~., data=as.data.frame(dataAll[,(ncol(y_I_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_I_train[,1],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
pred<-predict(mrt_i_spat[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_I[m,2]<-confusionMatrix(data=pred,reference=dada_I)$overall[1]
	} 
	if (ncol(S_I_train)==2) {
mrt_i_spat1[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_I_train)])~., data=as.data.frame(dataAll[,(ncol(y_I_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_I_train[,1],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
mrt_i_spat2[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_I_train)])~., data=as.data.frame(dataAll[,(ncol(y_I_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_I_train[,2],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
pred<-predict(mrt_i_spat1[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_I[m,3]<-confusionMatrix(data=pred,reference=dada_I)$overall[1]
pred<-predict(mrt_I_spat2[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_I[m,4]<-confusionMatrix(data=pred,reference=dada_I)$overall[1]
	}
}
eT<-Sys.time()
comTimes$MRTS_spat<-eT-sT

mrt_I <- mrt_I[[which(mrtAccuracy_I==max(mrtAccuracy_I[,1]),arr.ind=T)[1]]]
if (ncol(S_I_train)==1) {
mrt_I_spat <- mrt_i_spat[[which(mrtAccuracy_I==max(mrtAccuracy_I[,2]),arr.ind=T)[1]]]
}
if (ncol(S_I_train)==2) {
mrt_I_spat[[1]] <- mrt_i_spat1[[which(mrtAccuracy_I==max(mrtAccuracy_I[,3]),arr.ind=T)[1]]]
mrt_I_spat[[2]] <- mrt_i_spat2[[which(mrtAccuracy_I==max(mrtAccuracy_I[,4]),arr.ind=T)[1]]]
mrt_I_spat<-mrt_I_spat[[which(cbind(max(mrtAccuracy_I[,3]),max(mrtAccuracy_I[,4]))==max(cbind(max(mrtAccuracy_I[,3]),max(mrtAccuracy_I[,4]))),arr.ind=T)[1]]]
}

save(mrt_I, file=paste(FD,set_no,"/mrt_I.r",sep=""))
save(mrt_I_spat, file=paste(FD,set_no,"/mrt_I_spat.r",sep=""))


# Extrapolation
##########################################################################################
expl_E		<- X_E_train
resp_E		<- y_E_train

dataAll<-as.data.frame(cbind(resp_E,expl_E))
colnames(dataAll)<-paste("V",1:ncol(dataAll),sep="")

nsp <- ncol(resp_E)
nsites <- nrow(dataAll)

mrt_E<-list()
mrt_E_spat<-list()
mrt_e<-list()
mrt_e_spat<-list()
mrt_e_spat1<-list()
mrt_e_spat2<-list()
mrtAccuracy_E<-matrix(NA, nrow=length(ntrees),ncol=4)
dada_E<-unlist(dataAll[,1:ncol(resp_E)])

for (m in 1:length(ntrees)){

mrt_e[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_E_train)])~., data=as.data.frame(dataAll[,(ncol(y_E_train)+2):(ncol(dataAll))]),
              size=ntrees[m],xv="lse",xval=10,xvmult=0,plot.add=FALSE,control=list(cp=0.001))

pred<-predict(mrt_E[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_E[m,1]<-confusionMatrix(data=pred,reference=dada_E)$overall[1]

if (ncol(S_E_train)==1) {
mrt_e_spat[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_E_train)])~., data=as.data.frame(dataAll[,(ncol(y_E_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_E_train[,1],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
pred<-predict(mrt_e_spat[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_E[m,2]<-confusionMatrix(data=pred,reference=dada_E)$overall[1]
} 
if (ncol(S_E_train)==2) {
mrt_e_spat1[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_E_train)])~., data=as.data.frame(dataAll[,(ncol(y_E_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_E_train[,1],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
mrt_e_spat2[[m]] <- mvpart(data.matrix(dataAll[,1:ncol(y_E_train)])~., data=as.data.frame(dataAll[,(ncol(y_E_train)+2):(ncol(dataAll))]),
              		size=ntrees[m],xv="lse",xval=S_E_train[,2],xvmult=0,plot.add=FALSE,control=list(cp=0.001))
pred<-predict(mrt_e_spat1[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_E[m,3]<-confusionMatrix(data=pred,reference=dada_E)$overall[1]
pred<-predict(mrt_E_spat2[[m]],type="matrix")
pred<-rbinom(length(pred),1,pred)
mrtAccuracy_E[m,4]<-confusionMatrix(data=pred,reference=dada_E)$overall[1]
}
}

mrt_E <- mrt_e[[which(mrtAccuracy_E==max(mrtAccuracy_E[,1]),arr.ind=T)[1]]]
if (ncol(S_E_train)==1) {
mrt_E_spat <- mrt_e_spat[[which(mrtAccuracy_E==max(mrtAccuracy_E[,2]),arr.ind=T)[1]]]
}
if (ncol(S_E_train)==2) {
mrt_E_spat[[1]] <- mrt_e_spat1[[which(mrtAccuracy_E==max(mrtAccuracy_E[,3]),arr.ind=T)[1]]]
mrt_E_spat[[2]] <- mrt_e_spat2[[which(mrtAccuracy_E==max(mrtAccuracy_E[,4]),arr.ind=T)[1]]]
mrt_E_spat<-mrt_E_spat[[which(cbind(max(mrtAccuracy_E[,3]),max(mrtAccuracy_E[,4]))==max(cbind(max(mrtAccuracy_E[,3]),max(mrtAccuracy_E[,4]))),arr.ind=T)[1]]]
}

save(mrt_E, file=paste(FD,set_no,"/mrt_E.r",sep=""))
save(mrt_E_spat, file=paste(FD,set_no,"/mrt_E_spat.r",sep=""))

##########################################################################################
