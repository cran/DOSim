getDensityFactor <-
function(term){
	if(!exists("DOSimEnv")) initialize()
	nchildren<-get("nchildren",env=DOSimEnv)
	nparents<-get("nparents",env=DOSimEnv)
	e<-nchildren[term] + nparents[term]	  
	betaParam<-get("betaParam",envir=DOSimEnv)
	E<-(1-betaParam)*get("Eavg",env=DOSimEnv)/e + betaParam
	E
}

