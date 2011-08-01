calcTermSim <-
function(term1, term2, method="JiangConrath", verbose=TRUE){
	if(!exists("DOSimEnv")) initialize_DOSimEnv()
	IC<-get("IC", envir=DOSimEnv)
	if(verbose)
		print(paste("Terms:",term1,",",term2,"( method:",method,")"))	
	if(method== "Resnik"){
		an=getMinimumSubsumer(term1,term2)
		if(an == "NA"){
			return(0)
		}else{
			return(IC[an])   
		}
	}	
	else if(method == "JiangConrath"){
		an=getMinimumSubsumer(term1,term2)
		if(an == "NA"){
			return(0)	
		}else{
			return(1 - min(1, -2*IC[an] + IC[term1] + IC[term2]) )	
		}
	}
	else if(method == "Lin"){
		an=getMinimumSubsumer(term1,term2)
		if(an == "NA"){
			return(0)	
		}else{
			res = 2*IC[an]/(IC[term1]+IC[term2])
			return(ifelse(is.na(res), 1, res))
		}	
	}
	#else if(method== "CoutoEnriched")
	#	return(getEnrichedSim(term1, term2))
	else if(method == "CoutoResnik")  
		return(getDisjCommAncSim(term1,term2, "Resnik"))
	else if(method == "CoutoJiangConrath")  
		return(getDisjCommAncSim(term1,term2, "JiangConrath"))
	else if(method == "CoutoLin"){
		res = getDisjCommAncSim(term1,term2, "Lin")
		return(ifelse(is.na(res), 1, res))
	}
	else if(method == "simIC"){ # Li et al.
		MICA = getMinimumSubsumer(term1,term2)
		if(MICA == "NA"){
			return(0)	
		}else{
			res = 2*IC[MICA]/(IC[term1]+IC[term2]) * (1 - 1/(1 + IC[MICA]))
			return(ifelse(is.na(res), 1, res))
		}
	}
	else if(method == "GIC") # graph information content
		return(getGIC(term1, term2))
	else if(method == "relevance"){ # Schlicker et al.
		MICA = getMinimumSubsumer(term1,term2)
		if(MICA == "NA"){
			return(0)	
		}else{
			res = (2*IC[MICA]/(IC[term1]+IC[term2]))*(1 - exp(-IC[MICA]))
			return(ifelse(is.na(res), 1, res))
		}	
	}else if(method == "Wang"){
		res=getSimWang(term1,term2)
		return(res)
	}
	else
		stop(paste("calcTermSim: Unknown term similarity",method))
}

