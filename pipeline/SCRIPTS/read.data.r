##########################################################################################
# READING *** REAL *** DATA
##########################################################################################

setwd(DD)

# data size
if (sz==3) { 
	samp<-customData[[1]]
	spSamp<-customData[[2]]
} else {
	samp<-1:dataN[sz] 	
}


# training
##########################################################################################
y_train<-list()
x_train<-list()
s_train<-list()

for (i in 1:3) {


y_train[[i]] <- read.csv(paste("Yt","_",i,"_",set_no,".csv",sep=""),header=FALSE)
if (sz==1 | sz==2) { spSamp<-1:ncol(y_train[[i]]) }
y_train[[i]] <- apply(y_train[[i]], 2, as.numeric)[samp,spSamp]

x_train[[i]] <- as.matrix(read.csv(paste("Xt","_",i,"_",set_no,".csv",sep=""),header=FALSE))
x_train[[i]] <- apply(x_train[[i]], 2, as.numeric)[samp,]

s_train[[i]] <- read.csv(paste("St","_",i,"_",set_no,".csv",sep=""),header=FALSE)
s_train[[i]] <- apply(s_train[[i]], 2, as.numeric)[samp,]

colnames(s_train[[i]])<-paste('Rand',1:ncol(s_train[[i]]),sep='')

ncovar<-ncol(x_train[[i]])
	
	for (k in 1:ncovar) {
		x_train[[i]]<-cbind(x_train[[i]],x_train[[i]][,k]^2)
	}

x_train[[i]]<-apply(x_train[[i]],2,scale)
x_train[[i]]<-cbind(1,x_train[[i]])
colnames(x_train[[i]])<-c('IC',paste('V',1:ncovar,sep=''),paste('V',1:ncovar,'_2',sep=''))

}

# validation
##########################################################################################
y_valid<-list()
x_valid<-list()
s_valid<-list()

for (i in 1:3) {

y_valid[[i]] <- read.csv(paste("Yv","_",i,"_",set_no,".csv",sep=""),header=FALSE)
if (sz==1 | sz==2) { spSamp<-1:ncol(y_valid[[i]]) }
y_valid[[i]] <- apply(y_valid[[i]], 2, as.numeric)[samp,spSamp]

x_valid[[i]] <- as.matrix(read.csv(paste("Xv","_",i,"_",set_no,".csv",sep=""),header=FALSE))
x_valid[[i]] <- apply(x_valid[[i]], 2, as.numeric)[samp,]

s_valid[[i]] <- read.csv(paste("Sv","_",i,"_",set_no,".csv",sep=""),header=FALSE)
s_valid[[i]] <- apply(s_valid[[i]], 2, as.numeric)[samp,]

colnames(s_valid[[i]])<-paste('Rand',1:ncol(s_valid[[i]]),sep='')

ncovar<-ncol(x_valid[[i]])

	for (k in 1:ncovar) {
		x_valid[[i]]<-cbind(x_valid[[i]],x_valid[[i]][,k]^2)
	}

x_valid[[i]]<-apply(x_valid[[i]],2,scale)
x_valid[[i]]<-cbind(1,x_valid[[i]])
colnames(x_valid[[i]])<-c('IC',paste('V',1:ncovar,sep=''),paste('V',1:ncovar,'_2',sep=''))

}

##########################################################################################

# Y_train<-list()
# Y_valid<-list()
# for (i in 1:3) {
# 	Y_train[[i]]<-y_train[[i]][,which(colSums(y_valid[[i]])!=0 & colSums(y_train[[i]])!=0,arr.ind=T)]
# 	Y_valid[[i]]<-y_valid[[i]][,which(colSums(y_valid[[i]])!=0 & colSums(y_train[[i]])!=0,arr.ind=T)]
# }
# rm(y_train)
# rm(y_valid)
# y_train<-Y_train
# y_valid<-Y_valid
# rm(Y_train)
# rm(Y_valid)

# new Y data files for HMSC-matlab
# for (i in 1:3) {
# 	write.table(y_train[[i]],file=paste("Yy_",set_no,"_",i,"_",dataN[sz],"_train.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")
# 	write.table(y_valid[[i]],file=paste("Yy_",set_no,"_",i,"_",dataN[sz],"_valid.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")
# 	}

##########################################################################################
# LISTS
##########################################################################################
DD_t<-list()
DD_v<-list()

for (i in 1:3) {

nsp <- ncol(y_train[[i]])

dd_t <- list()
for (j in 1:nsp) {
	dd_t[[j]] <- data.frame(cbind(y_train[[i]][,j],x_train[[i]],s_train[[i]]))
	colnames(dd_t[[j]]) <- c('sp',colnames(x_train[[i]]),colnames(s_train[[i]]))
	}	

dd_v <- list()
for (j in 1:nsp) {
	dd_v[[j]] <- data.frame(cbind(y_valid[[i]][,j],x_valid[[i]],s_valid[[i]]))
	colnames(dd_v[[j]]) <- c('sp',colnames(x_valid[[i]]),colnames(s_valid[[i]]))
	}	

DD_t[[i]]<-dd_t
DD_v[[i]]<-dd_v
}


##########################################################################################

# interp training data
# y_i_t <- y_I_train
# colnames(y_i_t) <- rep("sp", times=ncol(y_i_t))
# 
# interp valid data
# y_i_v <- y_I_valid
# colnames(y_i_v) <- rep("sp", times=ncol(y_i_v))
# 
# covariates
# x_i_t <- X_I_train
# x_i_v <- X_I_valid
# 
# spat
# s_i_t<-S_I_train
# s_i_v<-S_I_valid
# 
# extrap training data
# y_e_t <- y_E_train
# colnames(y_e_t) <- rep("sp", times=ncol(y_e_t))
# 
# extrap valid data
# y_e_v <- y_E_valid
# colnames(y_e_v) <- rep("sp", times=ncol(y_e_v))
# 
# covariates
# x_e_t <- X_E_train
# x_e_v <- X_E_valid
# 
# spat
# s_e_t<-S_E_train
# s_e_v<-S_E_valid
# 


