getShortestPath <-
function(term1,term2){
	if(term1 == term2){
		return(0)
	}
	if(!require(RBGL))
		stop("Package RBGL is required for function getShortestPath")
		
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
	ancestor<-get("ancestor",envir=DOSimEnv)
	an1<-unlist(ancestor[names(ancestor) == term1])
	an2<-unlist(ancestor[names(ancestor) == term2])
	ancommon = intersect(an1, an2)
	case1<-which(an2 == term1)  # term1 is an ancestor of term2
	case2<-which(an1 == term2) # term2 is an ancestor of term1
	dograph<-get("dograph",envir=DOSimEnv);
	
	if(length(case1) > 0){
		minp=sp.between(dograph,term2,term1)[[1]]$length
		return(minp);
	}else if(length(case2) > 0){
		minp=sp.between(dograph,term1,term2)[[1]]$length
		return(minp);
	}else if(length(ancommon)==0){
		return(Inf);
	}else{
		pathtmp<-vector("numeric",length=0);
		for(i in 1:length(ancommon)){
			pathtmp<-c(pathtmp,sp.between(dograph,term1,ancommon[i])[[1]]$length+sp.between(dograph,term2,ancommon[i])[[1]]$length);
		}
		pathtmp<-pathtmp[!is.na(pathtmp)]
		minp=min(pathtmp)
		return(minp)
	}		
}

