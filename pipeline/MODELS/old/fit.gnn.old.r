# fitting GNN models 
##########################################################################################

library("yaImpute")

# Interpolation
##########################################################################################
expl <- as.data.frame(X_I_train[,-1])
colnames(expl)<-paste("V",1:ncol(expl),sep="")
rownames(expl)<-paste("t",1:nrow(expl),sep="")
resp <- as.matrix(y_I_train)
rownames(resp)<-paste("t",1:nrow(resp),sep="")

sT<-Sys.time()
gnn_I <- yai(x=expl,y=resp,method="gnn",k=10)
eT<-Sys.time()
comTimes$GNN<-eT-sT

save(gnn_I, file=paste(FD,set_no,"/gnn_I.r",sep=""))

# Extrapolation
##########################################################################################

expl <- as.data.frame(X_E_train[,-1])
colnames(expl)<-paste("V",1:ncol(expl),sep="")
rownames(expl)<-paste("t",1:nrow(expl),sep="")
resp <- as.matrix(y_I_train)
rownames(resp)<-paste("t",1:nrow(resp),sep="")

gnn_E <- yai(x=expl,y=resp,method="gnn",k=10)

save(gnn_E, file=paste(FD,set_no,"/gnn_E.r",sep=""))

