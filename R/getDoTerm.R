getDoTerm <-
function(dolist){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	
	if(!exists("DOSimEnv")) initialize()	
	doterm<-get("doterm",envir=DOSimEnv)
	res<-doterm[dolist[dolist %in% names(doterm)]]
	notmatch<-dolist[! dolist %in% names(doterm)]
	if(length(notmatch)>0){
		warning(paste(" ===>", length(notmatch), "of", length(dolist), "DOIDs not mapped to current disease ontology\n"))
	}
	res
}

