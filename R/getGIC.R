getGIC <-
function(term1, term2){
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
	if(term1 == term2){
		return(1)
	}
	IC<-get("IC", envir=DOSimEnv)
	ancestor<-get("ancestor",envir=DOSimEnv)
	an1<-unlist(ancestor[names(ancestor) == term1])
	an2<-unlist(ancestor[names(ancestor) == term2])
	ancommon = intersect(an1, an2)
	anunion = union(an1, an2)
	v=sum(IC[ancommon]) / sum(IC[anunion]);
	if(is.nan(v)){
		v=0;
	}
	return(v)
}

