getSimPath <-
function(term1,term2){
	minp=getShortestPath(term1,term2)
	if(minp == 0 ){
			return(1);
	}else if (is.na(minp) || minp == Inf){
		return(0);
	}else{
		return(1/(minp+1));
	}
}

