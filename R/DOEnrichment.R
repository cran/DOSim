DOEnrichment <-
function(genelist,method="hypertest",filter=5,cutoff=0.05){
	#do some check first
	if(! method %in% c("hypertest","fisher"))
		stop(paste("Unkown method",method,"for DOEnrichment"))
	
	
	if(!exists("DOSimEnv")) initialize()	
	if(is.list(genelist)){
		genelist<-unique(unlist(genelist));
	}else{
		genelist<-unique(genelist);
	}	
	
	genelist<-as.character(genelist)
	######################################
	#get gene annotated  doterms
	domap<-get("domap",envir=DOSimEnv)
	
	annotateddoids<-domap[genelist[genelist %in% names(domap)]]
	annotateddoids<-unique(unlist(annotateddoids))
	if(length(annotateddoids) == 0){
		stop(paste("No DOIDS are annotated by",length(genelist),"gene list"))
	}
	
	
	doanno<-get("doanno",envir=DOSimEnv)
	######################################
	#filter out doid which have at least 5 geneids annotated to this term
	
	filtereddoids<-sapply(annotateddoids,function(x){length(doanno[[x]])>=filter})
	filtereddoids<-annotateddoids[filtereddoids]
	
	if(length(filtereddoids)==0){
		stop(paste("No DOIDS annotated by",length(genelist),"gene list match the requirement that have at least",filter,"genes annotated to the DO term"))
	}
	
	humangenenum<-get("humangenenum",envir=DOSimEnv)
	#humangenenum<-length(domap)
	res<-list()
	searchgenenum<-length(genelist)
	for(i in 1:length(filtereddoids)){
		n<-length(doanno[[filtereddoids[i]]]) #the doid annotated gene num
		m<-length(genelist[genelist %in% doanno[[filtereddoids[i]]]])
		if(method == "hypertest"){
			p=phyper(m,n,humangenenum-n,searchgenenum,lower.tail=FALSE)
		}else{
			dat<-matrix(c(m,n,searchgenenum,humangenenum),2)
			p<-fisher.test(dat)$p.value
		}
		
		if(p<=cutoff){
			res[[filtereddoids[i]]]<-list('doid'=filtereddoids[i],'pvalue'=p,'odds'=(m/searchgenenum)/(n/humangenenum),'genenum1'=n,'genenum2'=m)
		}
	}
	
	if(length(res)==0){
		res1=NULL;
	}else{
		res1<-data.frame("DOID"=sapply(res,function(x){x$doid}),"pvalue"=sapply(res,function(x){x$pvalue}),"odds"=sapply(res,function(x){x$odds}),"genenum1"=sapply(res,function(x){x$genenum1}),"genenum2"=sapply(res,function(x){x$genenum2}))
	}
	
	sort.data.frame <- function(x, key, ...) {
	    if (missing(key)) {
		rn <- rownames(x)
		if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
		x[order(rn, ...), , drop=FALSE]
	    } else {
		x[do.call("order", c(x[key], ...)), , drop=FALSE]
	    }
	}

	res1<-sort.data.frame(res1,"pvalue",decreasing=FALSE)
	res1
		
}

