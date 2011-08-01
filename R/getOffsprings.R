getOffsprings <-
function(dolist,verbose=TRUE){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	if(verbose){
		print("Start to fetch the offsprings")
	}
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
	offspring<-get("offspring",envir=DOSimEnv)
	res<-offspring[dolist[dolist %in% names(offspring)]]
	notmatch<-dolist[! dolist %in% names(offspring)]
	if(length(notmatch)>0){
		for(i in 1:length(notmatch)){
			res[[notmatch[i]]]<-NA
		}
	}
	res
}

