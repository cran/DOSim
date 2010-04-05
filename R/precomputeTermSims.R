precomputeTermSims <-
function(x, y=NULL, similarityTerm="JiangConrath", verbose=TRUE){ 	
	if(verbose)
		print("precomputing term similarities ...")	
	gotermsx<-as.vector(unique(unlist(sapply(x, function(xx) xx$annotation))))
	if(!is.null(y)){  		 
		gotermsy<-as.vector(unique(unlist(sapply(y, function(xx) xx$annotation))))
		
		STerm<-matrix(0, nrow=length(gotermsx), ncol=length(gotermsy))
		rownames(STerm)=gotermsx
		colnames(STerm)=gotermsy
		for(i in 1:length(gotermsx)){						
			for(j in 1:length(gotermsy)){          
				STerm[i,j]<-calcTermSim(gotermsx[i],gotermsy[j], similarityTerm, verbose)
			}			
		}		
	} else{
		STerm = getTermSim(gotermsx, method=similarityTerm,verbose=verbose)			
		return(STerm)
	}	
	STerm
}

