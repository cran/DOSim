getAncestors <-
function(dolist,verbose=TRUE){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	if(verbose){
		print("Start to fetch the ancestors")
	}
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
	ancestor<-get("ancestor",envir=DOSimEnv)
	res<-ancestor[dolist[dolist %in% names(ancestor)]]
	notmatch<-dolist[! dolist %in% names(ancestor)]
	if(length(notmatch)>0){
		for(i in 1:length(notmatch)){
			res[[notmatch[i]]]<-NA
		}
	}
	res
}

