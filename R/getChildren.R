getChildren <-
function(dolist,verbose=TRUE){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	if(verbose){
		print("Start to fetch the children")
	}
	if(!exists("DOSimEnv")) initialize()	
	children<-get("children",envir=DOSimEnv)
	res<-children[dolist[dolist %in% names(children)]]
	notmatch<-dolist[! dolist %in% names(children)]
	if(length(notmatch)>0){
		for(i in 1:length(notmatch)){
			res[[notmatch[i]]]<-NA
		}
	}
	res
}

