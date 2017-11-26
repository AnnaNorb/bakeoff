# GLMs
##########################################################################################
library("pROC")

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

glm_I <- list()

bglm_I<-foreach(i=1:nsp) %dopar% { tryCatch({ glm(form, family=binomial(link = "logit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

sT<-Sys.time()
qbglm_I<-foreach(i=1:nsp) %dopar% { tryCatch({ glm(form, family=quasibinomial(link = "logit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$GLM<-eT-sT

	for (i in 1:length(bglm_I)) {
	aucs<-c(auc(dd_i_t[[i]][,1],predict(bglm_I[[i]])),auc(dd_i_t[[i]][,1],predict(qbglm_I[[i]])))
	if (aucs[1]>=aucs[2]) { glm_I[[i]]<-bglm_I[[i]] }	
	if (aucs[1]<aucs[2]) { glm_I[[i]]<-qbglm_I[[i]] }
	}
	
save(glm_I, file=paste(FD,set_no,"/glm_I.r",sep=""))


### EXTRAPOLATION
##########################################################################################
nsp <- length(dd_e_t)

glm_E <- list()
glmer_E <- list()

bglm_E<-foreach(i=1:nsp) %dopar% { tryCatch({ glm(form, family=binomial(link = "logit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
qbglm_E<-foreach(i=1:nsp) %dopar% { tryCatch({ glm(form, family=quasibinomial(link = "logit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	for (i in 1:length(bglm_E)) {
	aucs<-c(auc(dd_e_t[[i]][,1],predict(bglm_E[[i]])),auc(dd_e_t[[i]][,1],predict(qbglm_E[[i]])))
	if (aucs[1]>=aucs[2]) { glm_E[[i]]<-bglm_E[[i]] }	
	if (aucs[1]<aucs[2]) { glms_E[[i]]<-qbglm_E[[i]] }
	}
	
save(glm_E, file=paste(FD,set_no,"/glm_E.r",sep=""))

##########################################################################################