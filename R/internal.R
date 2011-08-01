#!/usr/bin/R
###################################
#Author :Jiang Li
#Email  :riverlee2008@gamil.com
#MSN    :riverlee2008@live.cn
#Address:Harbin Medical University
#TEl    :+86-13936514493
###################################

#Author:James Z. Wang
#Title:A new method to measure the semantic similarity of GO terms
#Date:2007
#Journal:Bioinformatics
#Formular:(sum(Sa(t)+Sb(t))/(SV(a)+SV(b))
getSimWang<-function(term1,term2){
	if(!exists("DOSimEnv")) initialize_DOSimEnv()
	
	if(term1 == term2){
		return(1);
	}
	ancestor<-get("ancestor",envir=DOSimEnv)
	#we<-get("we",envir=DOSimEnv)
	we=0.7
	an1<-unique(unlist(ancestor[names(ancestor) == term1]))
	an2<-unique(unlist(ancestor[names(ancestor) == term2]))
	
	an1<-c(an1,term1)
	an2<-c(an2,term2)
	
	an1<-unique(an1)
	an2<-unique(an2)
	common<-intersect(an1,an2)
	if(length(common)==0){
		return(0);
	}
	
	SA<-list()
	SB<-list()
	SA[term1]=1;
	SB[term2]=1;
	
	done <- FALSE
	while (!done) {
		if(all(an1 %in% names(SA))){
			done=TRUE;
		}else{
			parents<-unique(unlist(getParents(names(SA),verbose=FALSE)))
			parents<-parents[!is.na(parents)]
			v<-parents[! parents %in% names(SA)]
			if(length(v) == 0){
				done=TRUE;
			}else{
				vv<-v[1]
				vvchildren<-unique(unlist(getChildren(vv,verbose=FALSE)))
				s=max(sapply(SA[vvchildren[vvchildren %in% names(SA)]],function(x){we*x}))
				SA[vv]<-s	
			}
			
			
		}
	}
	
	done<-FALSE
	while (!done) {
		if(all(an2 %in% names(SB))){
			done=TRUE;
		}else{
			parents<-unique(unlist(getParents(names(SB),verbose=FALSE)))
			parents<-parents[!is.na(parents)]
			v<-parents[! parents %in% names(SB)]
			if(length(v) == 0){
				done=TRUE;
			}else{
				vv<-v[1]
				vvchildren<-unique(unlist(getChildren(vv,verbose=FALSE)))
				s=max(sapply(SB[vvchildren[vvchildren %in% names(SB)]],function(x){we*x}))
				SB[vv]<-s	
			}
			
			
		}
	}
	
	SVA= sum(unlist(SA))
	SVB= sum(unlist(SB))
	res<-(sum(unlist(SA[common]))+sum(unlist(SB[common])))/(SVA+SVB)
	
	res	
	
}


