getDisjAnc <-
function(term, an){		
	if(!require(RBGL))
		stop("Package RBGL is required for function getDisjAnc")
	G<-getDOGraph(term)	
	disan<-matrix(0,ncol=2,nrow=0)
	for(n1 in 1:length(an)){
		if(n1 > 1){
			for(n2 in 1:(n1-1)){
				if(!separates(term, an[n1], an[n2], G) && !separates(term, an[n2], an[n1], G))
					disan<-rbind(disan,c(an[n1], an[n2]))
			}
		}
	}
	disan
}

