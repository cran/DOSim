getEnrichedSim <-
function(term1, term2){   
	if(!require(RBGL))
		stop("Package RBGL is required for function getDepthFactor")
	if(!exists("DOSimEnv")) initialize() 
	ms<-getMinimumSubsumer(term1,term2)
	
	if(is.na(ms) || ms == 'NA'){
		return(0)
	}
	
	IC<-get("IC", envir=DOSimEnv)	
	#load("ICsDOhumanall.rda")
	if(term1 != term2){    	    	
		G<-getDOGraph(c(term1,term2))
		if(term1 != ms){
			path1=sp.between(G,term1,ms)[[1]]$path # path to ms                
			len<-length(path1)
			delta1<-sum(sapply(path1[2:len],getDepthFactor,G)*sapply(path1[2:len],getDensityFactor)*(-diff(IC[path1])))
		}
		else
			delta1<-0
		if(term2 != ms){
			path2<-sp.between(G,term2,ms)[[1]]$path # path to ms    	
			len<-length(path2)
			delta2<-sum(sapply(path2[2:len],getDepthFactor,G)*sapply(path2[2:len],getDensityFactor)*(-diff(IC[path2])))
		}
		else
			delta2<-0
		delta<-delta1 + delta2		
		sim<-1 - min(1, delta)
	}
	else
		sim<-1 
	sim<-sim * IC[term1] * IC[term2]  # correction given in equation (11) of the FuSSiMeg paper
	names(sim)<-c()
	if(is.nan(sim) || sim == Inf){
		sim=0
	}   
	sim
}

