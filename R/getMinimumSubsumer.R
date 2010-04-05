getMinimumSubsumer <-
function(term1, term2){	
	if(!exists("DOSimEnv")) initialize()
	ancestor<-get("ancestor",envir=DOSimEnv)
	#load("DOAncestor.rda")
	if(term1 == term2){
		ms<-term1	
		return(ms)
	}
	an1<-unlist(ancestor[names(ancestor) == term1])
	an2<-unlist(ancestor[names(ancestor) == term2])
	case1<-which(an2 == term1)  # term1 is the ms of term2
	case2<-which(an1 == term2) # term2 is the ms of term1	  
	if(length(case1) > 0){
		ms<-term1	
	} else if(length(case2) > 0) {
		ms<-term2	
	} else {
		# find common ancestor with maximal information content
		anall<-intersect(an1, an2) 
		IC<-get("IC", envir=DOSimEnv)
		ms<-anall[which.max(IC[anall])]
	}	
	if(is.null(ms) | length(ms) == 0)
		ms <- "NA"	
	ms
}

