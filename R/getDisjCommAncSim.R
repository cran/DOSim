getDisjCommAncSim<-function(term1, term2, method="JiangConrath"){
	if(!exists("DOSimEnv")) initialize()
	IC<-get("IC", envir=DOSimEnv)
	djca<-getDisjCommAnc(term1, term2)	
	ICdjca<-IC[djca]	
	ICdjca<-ICdjca[!is.na(ICdjca)]							
	ICshare<-mean(ICdjca)
	if(is.na(ICshare)){
		return(0)
	}		
	if(method == "JiangConrath")
		return(1-min(1,-2*ICshare + IC[term1] + IC[term2]))
	else if(method == "Resnik")
		return(ICshare)
	else if(method == "Lin")
		return(2*ICshare/(IC[term1]+IC[term2]))
	else
		stop(paste("getDisjCommAnc: Unknown term similarity",method))
}
