# GLMs, also with SPATIAL STRUCTURE
##########################################################################################
library('nlme')
library('MASS')

if (set_no=="butterfly"|set_no=="bird") { 
form<-as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
}
if (set_no=="diatom") { 
form<-as.formula(sp~V1+V2+V1_2+V2_2)
}
if (set_no=="tree"|set_no=="plant") {
form<-as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2)
}
if (set_no=="fungi") {
form<-as.formula(sp~V1+V2+V3+V2_2+V3_2)
}


### INTERPOLATION
##########################################################################################
nsp <- length(dd_i_t)

dada<-lapply(dd_i_t,cbind,1:nrow(dd_i_t[[1]]))
for (i in 1:nsp) { colnames(dada[[i]])[ncol(dada[[i]])] <- "ID" }

glmmpql_I <- list()
glmmpql_spat_I <- list()

if (set_no=="fungi") { 
	sT<-Sys.time()
	for (i in 1:nsp) {
	glmmpql_I <- tryCatch({ glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]])}, 
					error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	save(glmmpql_I, file=paste(FD,set_no,"/glmmPQL/glmmpql_I",i,".r",sep=""))
	}
	eT<-Sys.time()
	comTimes$glmPQL<-eT-sT
} else {	
	sT<-Sys.time()
	glmmpql_I<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	eT<-Sys.time()
	comTimes$glmPQL<-eT-sT
	save(glmmpql_I, file=paste(FD,set_no,"/","glmmpql_I.r",sep=""))
}

if (set_no=="fungi") { 
	sT<-Sys.time()
	for (i in 1:nsp) {
		glmmpql_spat_I<- tryCatch({ glmmPQL(form, random= ~ 1|Rand1, family=binomial(link = "probit"), data=dd_i_t[[i]])}, 
						  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		save(glmmpql_spat_I, file=paste(FD,set_no,"/glmmPQL/glmmpql_spat_I",i,".r",sep=""))
	}
	eT<-Sys.time()
	comTimes$glmPQLspat<-eT-sT
} 
if (set_no=="diatom") { 
	sT<-Sys.time()
	glmmpql_spat_I<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~ 1|Rand1, family=binomial(link = "probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	eT<-Sys.time()
	comTimes$glmPQLspat<-eT-sT
	save(glmmpql_spat_I, file=paste(FD,set_no,"/glmmpql_spat_I.r",sep=""))
} else {	
	sT<-Sys.time()
	glmmpql_spat_I<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~ 1|IC, correlation =  corExp(form = ~ Rand1 + Rand2), family=binomial(link = "probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	eT<-Sys.time()
	comTimes$glmPQLspat<-eT-sT
	save(glmmpql_spat_I, file=paste(FD,set_no,"/glmmpql_spat_I.r",sep=""))
}



### EXTRAPOLATION
##########################################################################################
nsp <- length(dd_e_t)

dada<-lapply(dd_e_t,cbind,1:nrow(dd_e_t[[1]]))
for (i in 1:nsp) { colnames(dada[[i]])[ncol(dada[[i]])] <- "ID" }

glmmpql_E <- list()
glmmpql_spat_E <- list()

if (set_no=="fungi") { 
	for (i in 1:nsp) {
	glmmpql_E <- tryCatch({ glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	save(glmmpqls_E, file=paste(FD,set_no,"/glmmPQL/glmmpql_E",i,".r",sep=""))
	}
} else {	
	glmmpql_E<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	save(glmmpql_E, file=paste(FD,set_no,"/glmmpql_E.r",sep=""))
}

if (set_no=="fungi") { 
	for (i in 1:nsp) {
		glmmpql_spat_E<- tryCatch({ glmmPQL(form, random= ~ 1|Rand1, family=binomial(link = "probit"), data=dd_e_t[[i]])}, 
						  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		save(glmmpql_spat_E, file=paste(FD,set_no,"/glmmPQL/glmmpql_spat_E",i,".r",sep=""))
	}
} 
if (set_no=="diatom") { 
	glmmpql_spat_E<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~ 1|Rand1, family=binomial(link = "probit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	save(glmmpql_spat_E, file=paste(FD,set_no,"/glmmpql_spat_E.r",sep=""))
} else {	
	glmmpql_spat_E<-foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ glmmPQL(form, random= ~ 1|IC, correlation =  corExp(form = ~ Rand1 + Rand2), family=binomial(link = "probit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	save(glmmpql_spat_E, file=paste(FD,set_no,"/","glmmpql_spat_E.r",sep=""))
}

##########################################################################################