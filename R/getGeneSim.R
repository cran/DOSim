getGeneSim <-
function(genelist, similarity="funSimMax", similarityTerm="relevance", normalization=TRUE, method="sqrt", avg=(similarity=="OA"), verbose=TRUE){
	genelist <- unique(genelist)
	if(length(genelist) < 2)
		stop("Gene list should contain more than 2 elements!")
		
	allgenes<-filterDO(genelist)	
	if(length(allgenes) > 1){
		
		if(!(similarity %in% c("dot")))
			STerm<-precomputeTermSims(x=allgenes, similarityTerm=similarityTerm, verbose=verbose) # precompute term similarities => speed up!		
		else
			STerm = NULL
		
		if(verbose)
			print(paste("Calculating similarity matrix with similarity measure",similarity))
		
		Ker<-matrix(0,nrow=length(allgenes),ncol=length(allgenes))
		colnames(Ker)<-sapply(allgenes,function(x) x$genename)
		rownames(Ker)<-colnames(Ker)
		
		for(i in 1:length(allgenes)){
			annoi<-(allgenes[[i]]$annotation)		
			Ker[i,i]<-getGSim(annoi,annoi, similarity, similarityTerm, STerm=STerm, avg=avg, verbose)
			if(i > 1){
				for(j in 1:(i-1)){
					annoj<-(allgenes[[j]]$annotation)
	# 			        print(paste(allgenes[[i]]$genename,allgenes[[j]]$genename))
					Ker[i,j]<-getGSim(annoi,annoj, similarity, similarityTerm, STerm=STerm, avg=avg, verbose)
					Ker[j,i]<-Ker[i,j]
				}
			}
		}
					
		if(normalization){			
			Ker = normalize.kernel(Ker, method)
		}			
	}
	else{
		if(length(allgenes) == 0)
			stop("No gene has DO information!")					
		else if(length(allgenes) == 1)
			stop(paste("Only gene",allgenes," has DO information!"))					
	}
	Ker
}

