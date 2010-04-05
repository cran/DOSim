getGeneFeatures.internal <-
function(anno){
	ancestor<-get("ancestor",envir=DOSimEnv)	
	an<-unlist(ancestor[names(ancestor) %in% anno])	
	IC<-get("IC", envir=DOSimEnv)
	v = double(length(IC))	
	names(v) = names(IC)	
	v[c(anno, an)] = IC[c(anno, an)]
	v
}

