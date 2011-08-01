getDoAnno <-
function(dolist){
	if(is.list(dolist)){
		dolist<-unique(unlist(dolist))
	}else{
		dolist<-unique(dolist)
	}
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
	doanno<-get("doanno",envir=DOSimEnv)
	res<-doanno[dolist[dolist %in% names(doanno)]]
	notmatch<-dolist[! dolist %in% names(doanno)]
	if(length(notmatch)>0){
		warning(paste(" ===>", length(notmatch), "of", length(dolist), "DOIDs not mapped to current disease ontology\n"))
	}
	res
	
}

