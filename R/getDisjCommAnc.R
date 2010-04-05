getDisjCommAnc <-
function(term1, term2){
	if(!exists("DOSimEnv")) initialize()
	ancestor<-get("ancestor",envir=DOSimEnv)
	#load("DOAncestor.rda")
	IC<-get("IC", envir=DOSimEnv)
	#load("ICsDOhumanall.rda")
	if(term1 == term2){
		return(term1)
	}
	else{
		an1<-unlist(ancestor[names(ancestor) == term1])
		an2<-unlist(ancestor[names(ancestor) == term2])
		case1<-which(an2 == term1)  # term1 is an ancestor of term2
		case2<-which(an1 == term2) # term2 is an ancestor of term1
		if(length(case1) > 0){
			ancommon<-an1
			andisj<-getDisjAnc(term1, an1)
		}
		else if(length(case2) > 0){			
			ancommon<-an2
			andisj<-getDisjAnc(term2, an2)			
		}
		else{
			ancommon<-intersect(an1,an2)
			andisj<-getDisjAnc(c(term1,term2), ancommon) # we only need to calculate the disjunctives among the common ancestors!			
		}		
		djca<-c()
		cond1<-sapply(ancommon, function(x) setdiff(ancommon[which(IC[ancommon] >= IC[x])],x)) # which common ancestors are more informative than a_i?
		if(length(cond1) > 0){ # look for those that are disjunctive
			names(cond1)<-ancommon					
			for(i in 1:length(cond1)){
				res<-sapply(cond1[i][[1]],function(x) any(andisj[,1] == names(cond1[i]) & andisj[,2] == x) | any(andisj[,2] == names(cond1[i]) & andisj[,1] == x))				
				if(all(res))
					djca<-c(djca, names(cond1[i]))
			}
			djca= unique(djca)			
		}	
		if(length(djca)==0)
			djca<-ancommon[which.max(IC[ancommon])]# take minimum subsumer otherwise		
		return(djca)		
	}	
}

