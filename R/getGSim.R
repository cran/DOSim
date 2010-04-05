getGSim <-
function(anno1, anno2, similarity="max", similarityTerm="JiangConrath", STerm=NULL, avg=FALSE, verbose=TRUE){	  
  if(length(anno1) < length(anno2)){
	a1<-anno1
	a2<-anno2
	swap<-FALSE
  }
  else{
	a1<-anno2
	a2<-anno1
	swap<-TRUE
  }
  		
  if(!is.null(STerm)){ # use precomputed similarity values
	if(!swap)
		ker<-STerm[a1,a2]
	else
		ker<-STerm[a2,a1]
			
	if(length(a1) == 1)
		ker<-t(as.matrix(ker))		
	if(is.null(ker) || is.null(nrow(ker))){
		warning(paste("No DO information for",a1,a2,". Similarity set to NaN."))		
		return(NaN)
	}	
	
	if(nrow(ker) > ncol(ker))
		ker<-t(ker)
  }
  else{ 
	if(similarity %in% c("dot"))
		return(getWeightedDotSim(a1, a2))			
	else{
		# calculate term similarity
		ker<-matrix(0,nrow=length(a1),ncol=length(a2))	
		for(i in 1:length(a1)){
			for(j in 1:length(a2))
				ker[i,j]<-calcTermSim(a1[i],a2[j], similarityTerm, verbose)		
		}
	}
  }
    
  if(length(a1)*length(a2) > 0){
	if(similarity == "OA"){				
		res<-.C("OAWrapper", ker, nrow(ker), ncol(ker), as.integer(1), ret=double(1))$ret
		if(avg)
			res = res/length(a2)	
		return(res)
	}
	else if(similarity == "max"){				
		return(max(ker))
	}
	else if(similarity == "mean"){				
		return(mean(ker))
	}  
	else if(similarity == "funSimAvg"){
		rowMax = mean(apply(ker,1,max))
		colMax = mean(apply(ker,2,max))
		return(0.5*(rowMax + colMax))
	}
	else if(similarity == "funSimMax"){
		rowMax = mean(apply(ker,1,max))
		colMax = mean(apply(ker,2,max))
		return(max(rowMax, colMax))
	}
	else if(similarity == "hausdorff"){
		rowMax = min(apply(ker,1,max))
		colMax = min(apply(ker,2,max))
		return(min(rowMax, colMax))	
	}else if(similarity =="Wang"){
		m=nrow(ker)
		n=ncol(ker)
		return((sum(apply(ker,1,max))+sum(apply(ker,2,max)))/(m+n))
	}	
	else
		stop(paste("getDSim: Unknown gene similarity",similarity,"!"))
  }
  else{	
	warning(paste("No DO information for",a1,a2,". Similarity set to NaN."))		
	return(NaN)
  }
}

