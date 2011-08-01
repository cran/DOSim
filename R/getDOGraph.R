getDOGraph <-
function(term, prune=Inf){
	if(!require(graph))
		stop("Package graph is required for function getDOGraph")
	if(!exists("DOSimEnv")) initialize_DOSimEnv()
	ENV_Child2Parent<-get("ENV_Child2Parent",envir=DOSimEnv);
	
	G<-DOGraph(term,ENV_Child2Parent)		
	if(prune != Inf){
		dis = johnson.all.pairs.sp(G)		
		inc = unique(unlist(sapply(term, function(t) names(dis[t,])[dis[t,] < prune])))
		G = subGraph(nodes(G)[inc], G)
	}		
	G
}

