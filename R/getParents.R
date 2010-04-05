getParents <-
function(dolist,verbose=TRUE){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	if(verbose){
		print("Start to fetch the parents")
	}
	if(!exists("DOSimEnv")) initialize()
	ENV_Child2Parent<-get("ENV_Child2Parent",envir=DOSimEnv);
	res<-mget(dolist,ENV_Child2Parent,ifnotfound=NA)
	res
}

