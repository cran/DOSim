filterDO <-
function(genelist){	
	cat("filtering out genes not mapping to the currently Disease Ontology ...")
	if(!exists("DOSimEnv")) initialize()	
	IC<-get("IC", envir=DOSimEnv)
	ids<-names(IC[IC != Inf]) # only consider DO terms with some known annotation   
	domap<-get("domap",env=DOSimEnv)
	k<-1
	allgenes<-list()
	for(i in 1:length(genelist)){		
		annoi<-domap[[match(as.character(genelist[i]), names(domap))]]
		annoi<-intersect(annoi, as.character(ids))				
		if(length(annoi) > 0){
			allgenes[[k]]<-list(annotation=annoi,genename=as.character(genelist[i]))
			k<-k + 1
		}
	}	
	cat(" ===> list of ", length(genelist), "genes reduced to ", length(allgenes), "\n")
	allgenes
}

