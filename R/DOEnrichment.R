#DOEnrichment <-
#function(genelist,method="hypertest",filter=5,cutoff=0.05){
#	#do some check first
#	if(! method %in% c("hypertest","fisher"))
#		stop(paste("Unkown method",method,"for DOEnrichment"))
#	
#	
#	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
#	if(is.list(genelist)){
#		genelist<-unique(unlist(genelist));
#	}else{
#		genelist<-unique(genelist);
#	}	
#	
#	genelist<-as.character(genelist)
#	######################################
#	#get gene annotated  doterms
#	domap<-get("domap",envir=DOSimEnv)
#	
#	annotateddoids<-domap[genelist[genelist %in% names(domap)]]
#	annotateddoids<-unique(unlist(annotateddoids))
#	if(length(annotateddoids) == 0){
#		stop(paste("No DOIDS are annotated by",length(genelist),"gene list"))
#	}
#	
#	
#	doanno<-get("doanno",envir=DOSimEnv)
#	######################################
#	#filter out doid which have at least 5 geneids annotated to this term
#	
#	filtereddoids<-sapply(annotateddoids,function(x){length(doanno[[x]])>=filter})
#	filtereddoids<-annotateddoids[filtereddoids]
#	
#	if(length(filtereddoids)==0){
#		stop(paste("No DOIDS annotated by",length(genelist),"gene list match the requirement that have at least",filter,"genes annotated to the DO term"))
#	}
#	
#	humangenenum<-get("humangenenum",envir=DOSimEnv)
#	#humangenenum<-length(domap)
#	res<-list()
#	searchgenenum<-length(genelist)
#	for(i in 1:length(filtereddoids)){
#		n<-length(doanno[[filtereddoids[i]]]) #the doid annotated gene num
#		m<-length(genelist[genelist %in% doanno[[filtereddoids[i]]]])
#		if(method == "hypertest"){
#			p=phyper(m,n,humangenenum-n,searchgenenum,lower.tail=FALSE)
#		}else{
#			dat<-matrix(c(m,n,searchgenenum,humangenenum),2)
#			p<-fisher.test(dat)$p.value
#		}
#		
#		if(p<=cutoff){
#			res[[filtereddoids[i]]]<-list('doid'=filtereddoids[i],'pvalue'=p,'odds'=(m/searchgenenum)/(n/humangenenum),'genenum1'=n,'genenum2'=m)
#		}
#	}
#	
#	if(length(res)==0){
#		res1=NULL;
#	}else{
#		res1<-data.frame("DOID"=sapply(res,function(x){x$doid}),"pvalue"=sapply(res,function(x){x$pvalue}),"odds"=sapply(res,function(x){x$odds}),"genenum1"=sapply(res,function(x){x$genenum1}),"genenum2"=sapply(res,function(x){x$genenum2}))
#	}
#	
#	sort.data.frame <- function(x, key, ...) {
#	    if (missing(key)) {
#		rn <- rownames(x)
#		if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
#		x[order(rn, ...), , drop=FALSE]
#	    } else {
#		x[do.call("order", c(x[key], ...)), , drop=FALSE]
#	    }
#	}
#
#	res1<-sort.data.frame(res1,"pvalue",decreasing=FALSE)
#	res1
#		
#}

#new version: timestamp:2010年06月27日 星期日 20时50分55秒 
#p.adjust(p, method = p.adjust.methods, n = length(p))
#p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
#DOEnrichment <-
#function(genelist,method="hypertest",filter=5,cutoff=0.05,adjustp="fdr"){
#	#do some check first
#	require(stats)
#	if(! method %in% c("hypertest","fisher"))
#		stop(paste("Unkown method",method,"for DOEnrichment"))
#	
#	if(!adjustp %in% p.adjust.methods)
#		adjustp="fdr"           #as default
#	
#	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
#	if(is.list(genelist)){
#		genelist<-unique(unlist(genelist));
#	}else{
#		genelist<-unique(genelist);
#	}	
#	
#	genelist<-as.character(genelist)
#	######################################
#	#get gene annotated  doterms
#	domap<-get("domap",envir=DOSimEnv)
#	
#	annotateddoids<-domap[genelist[genelist %in% names(domap)]]
#	annotateddoids<-unique(unlist(annotateddoids))
#	if(length(annotateddoids) == 0){
#		stop(paste("No DOIDS are annotated by",length(genelist),"gene list"))
#	}
#	
#	
#	doanno<-get("doanno",envir=DOSimEnv)
#	######################################
#	#filter out doid which have at least 5 geneids annotated to this term
#	
#	filtereddoids<-sapply(annotateddoids,function(x){length(doanno[[x]])>=filter})
#	filtereddoids<-annotateddoids[filtereddoids]
#	
#	if(length(filtereddoids)==0){
#		stop(paste("No DOIDS annotated by",length(genelist),"gene list match the requirement that have at least",filter,"genes annotated to the DO term"))
#	}
#	
#	humangenenum<-get("humangenenum",envir=DOSimEnv)
#	#humangenenum<-length(domap)
#	res<-list()
#	searchgenenum<-length(genelist)
#	for(i in 1:length(filtereddoids)){
#		n<-length(doanno[[filtereddoids[i]]]) #the doid annotated gene num
#		m<-length(genelist[genelist %in% doanno[[filtereddoids[i]]]])
#		if(method == "hypertest"){
#			p=phyper(m,n,humangenenum-n,searchgenenum,lower.tail=FALSE)
#		}else{
#			dat<-matrix(c(m,n,searchgenenum,humangenenum),2)
#			p<-fisher.test(dat)$p.value
#		}
#		
#		
#		res[[filtereddoids[i]]]<-list('doid'=filtereddoids[i],'pvalue'=p,'odds'=(m/searchgenenum)/(n/humangenenum),'genenum1'=n,'genenum2'=m)
#	}
#	
#	if(length(res)==0){
#		res1=NULL;
#	}else{
#		res1<-data.frame("DOID"=sapply(res,function(x){x$doid}),"Term"=sapply(res,function(x){getDoTerm(x$doid)[[1]]}),"genenum1"=sapply(res,function(x){x$genenum1}),"genenum2"=sapply(res,function(x){x$genenum2}),"odds"=sapply(res,function(x){x$odds}),"pvalue"=sapply(res,function(x){x$pvalue}))
#	}
#	
#	ajustp=p.adjust(res1$pvalue,method=adjustp)
#	
#	res2<-data.frame(res1,"adjustedP"=ajustp)
#	sort.data.frame <- function(x, key, ...) {
#	    if (missing(key)) {
#		rn <- rownames(x)
#		if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
#		x[order(rn, ...), , drop=FALSE]
#	    } else {
#		x[do.call("order", c(x[key], ...)), , drop=FALSE]
#	    }
#	}
#
#	res2<-sort.data.frame(res2,"pvalue",decreasing=FALSE)
#	res2[res2$adjustedP<=cutoff,]
#		
#}

getDefaultBackground<-function(){
     if(!exists("DOSimEnv")) initialize_DOSimEnv()	
     domap<-get("domap",envir=DOSimEnv)
      return(names(domap))
}


DOEnrichment <-
function(genelist,filter=5,cutoff=0.05,layer=5,backgroud=getDefaultBackground()){
	#do some check first
	#require(fdrtool)
	#adjustp="fdr"           #as default
	
	if(!exists("DOSimEnv")) initialize_DOSimEnv()	
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
	
	if(!is.null(layer) && !is.na(layer) && layer>1){
		dograph<-get("dograph",envir=DOSimEnv);
		index<-sapply(filtereddoids,function(x){sp.between(dograph,x,"DOID:4")[[1]]$length>=layer})
		filtereddoids<-filtereddoids[index]
	}
	
	if(length(filtereddoids)==0){
		stop(paste("No DOIDS annotated by",length(genelist),"gene list match the requirement that have at least",filter,"genes annotated to the DO term and from layer",layer))
	}
	
	#######################################
	#filter do ids using layer filter
	
	
	#humangenenum<-get("humangenenum",envir=DOSimEnv)
	#humangenenum<-length(domap)
	humangenenum<-length(backgroud)
	res<-list()
	searchgenenum<-length(genelist)
	for(i in 1:length(filtereddoids)){
		n<-length(doanno[[filtereddoids[i]]]) #the doid annotated gene num
		m<-length(genelist[genelist %in% doanno[[filtereddoids[i]]]])
		#p=phyper(m,n,humangenenum-n,searchgenenum,lower.tail=FALSE)
		p=1-phyper(m-1,n,humangenenum-n,searchgenenum)
				
		res[[filtereddoids[i]]]<-list('doid'=filtereddoids[i],'pvalue'=p,'odds'=(m/searchgenenum)/(n/humangenenum),'annGeneNumber'=m,'annBgNumber'=searchgenenum,'geneNumber'=n,'bgNumber'=humangenenum)
	}
	
	if(length(res)==0){
		res1=NULL;
		return(res1)
	}else{
		res1<-data.frame("DOID"=sapply(res,function(x){x$doid}),"Term"=sapply(res,function(x){getDoTerm(x$doid)[[1]]}),"annGeneNumber"=sapply(res,function(x){x$annGeneNumber}),"annBgNumber"=sapply(res,function(x){x$annBgNumber}),"geneNumber"=sapply(res,function(x){x$geneNumber}),"bgNumber"=sapply(res,function(x){x$bgNumber}),"odds"=sapply(res,function(x){x$odds}),"pvalue"=sapply(res,function(x){x$pvalue}))
	}
	
	#ajustp=p.adjust(res1$pvalue,method=adjustp)
	qvalueList=p.adjust(res1$pvalue,method="fdr")
	#qvalueList<-fdrtool(res1$pvalue, 
         #          statistic="pvalue",plot=FALSE,verbose=FALSE)$qval
	
	#res2<-data.frame(res1,"adjustedP"=ajustp)
	res2<-data.frame(res1,"qvalue"=qvalueList)
	sort.data.frame <- function(x, key, ...) {
	    if (missing(key)) {
		rn <- rownames(x)
		if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
		x[order(rn, ...), , drop=FALSE]
	    } else {
		x[do.call("order", c(x[key], ...)), , drop=FALSE]
	    }
	}

	res2<-sort.data.frame(res2,"qvalue",decreasing=FALSE)
	res2[res2$qvalue<=cutoff,]		
}


