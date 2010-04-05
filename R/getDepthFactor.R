getDepthFactor <-
function(term,G){	
	if(!require(RBGL))
		stop("Package RBGL is required for function getDepthFactor")
	if(!exists("DOSimEnv")) initialize()	
	ancestor<-get("ancestor",envir=DOSimEnv)
	firstlayer<-get("firstlayer",envir=DOSimEnv)
	if(term %in% firstlayer){
		d<-1
	}else{
		fathers<-ancestor[[term]]
		top<-fathers[fathers %in% firstlayer][1]
		if(is.na(top) || top == 'NA'){
			d<-1
		}else{
			d<-sp.between(G,term,top)[[1]]$length+1
		}
	}	
    	D<-((d+1)/d)^get("alphaParam",envir=DOSimEnv)
    	D
}

