getSimLch <-
function(term1,term2){
	minp=getShortestPath(term1,term2)
	if(!exists("DOSimEnv")) initialize()	
	depth<-get("depth",envir=DOSimEnv)
	if(is.na(minp) || minp == Inf){
		return(0);
	}else{
		return(-log((minp+1)/(2*depth)))
	}
}

