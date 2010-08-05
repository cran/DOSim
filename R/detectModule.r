#########################################
#use three method to detect module from a similarity matrix or a distance matrix
#module detection methods are: 1)constant; 2)tree; 3)hybrid
detectModule<- function(dat, isSimilarity=1, hierarchicalMethod="average", cutHeight = NULL, minClusterSize = 10, 
                       method = "hybrid", deepSplit = (ifelse(method=="hybrid", 1, FALSE)), 
                       maxCoreScatter = NULL, minGap = NULL,
                       maxAbsCoreScatter = NULL, minAbsGap = NULL, 
                       pamStage = TRUE, pamRespectsDendro = TRUE,
                       useMedoids = FALSE, maxDistToLabel = cutHeight,
                       respectSmallClusters = TRUE, 
                       verbose = 2, indent = 0){               
  	#method canbe constant, hybrid, or tree
	################################################
	#check whether dat is an matrix
	final=list()
	
	if(!is.matrix(dat)){
		stop("input dat must be a matrix object")
	}
	
	################################################
	#check wether it is a symmetrical matrix
	if( dim(dat)[1] != dim(dat)[2]){
		stop("input dat must be a symmetrical matrix")
	}	
	
	#################################################
	#check the size of the matrix
	if(dim(dat)[1]<minClusterSize){
		stop("input dat's size is less than minClusterSize")
	}
	
	if(isSimilarity){ #convert the similarity matrix "dat" to distM
		#first use Min-Max method to convert the similarity into range 0-1
		newdat<-(dat-min(dat))/(max(dat)-min(dat))
		diag(newdat)<-1
		distM<-1-newdat
		dendro<-hclust(as.dist(distM),method=hierarchicalMethod)		
	}else{
		#the dat matrix is alread an dist matrix
		newdat<-(dat-min(dat))/(max(dat)-min(dat))
		diag(newdat)<-0
		distM<-newdat
		dendro<-hclust(as.dist(distM),method=hierarchicalMethod)	
	}
	
  methods=c("constant","hybrid", "tree");
  met = charmatch(method, methods);
  
  if ( (met==1) && (is.null(cutHeight)) ){
    stop('cutreeConstant: method "constant" requires a valid height for tree cut "cutHeight".');
  }
  
  
  if (is.na(met)){
    stop(paste("Invalid method argument. Accepted values are (unique abbreviations of)", 
                paste(methods, collapse = ", ")));
  } else if (met==1) {
    	result=cutreeConstant(dendro=dendro,cutHeight=cutHeight,minClusterSize=minClusterSize)
    	final$module=result
  } else{
    if(!require("dynamicTreeCut")){
    	stop("package dynamicTreeCut is needed.")
    }
    result<-cutreeDynamic(dendro=dendro, cutHeight = cutHeight, minClusterSize = minClusterSize, 
                       method = method, distM = distM, deepSplit = deepSplit, 
                       maxCoreScatter = maxCoreScatter, minGap = minGap,
                       maxAbsCoreScatter = maxAbsCoreScatter, minAbsGap = minAbsGap, 
                       pamStage = pamStage, pamRespectsDendro = pamRespectsDendro,
                       useMedoids = useMedoids, maxDistToLabel = maxDistToLabel,
                       respectSmallClusters = respectSmallClusters, 
                       verbose = verbose, indent = indent)
    result<-as.numeric(result)
    names(result)<-dendro$label
    
    
    final$module=result
  }
  
  final$dendrogram=dendro
  
  #added on 2010-08-04 for further analysis
  final$simmatrix=1-distM
  
  return(final)

}




##########################################
#constant-height cut to detect module 
cutreeConstant<-function(dendro,cutHeight=0.9,minClusterSize=10){
       labelpred= cutree(dendro,h=cutHeight)
       sort1=-sort(-table(labelpred))  #larger to small
      	modulename= as.numeric(names(sort1))
      	modulebranch= sort1>minClusterSize
      	no.modules=sum(modulebranch)
      	#re-assignment modulename, 1 means the largest module, 2 the seconde largest ...
      	colorcode=1:no.modules
	# "0" means not in any module;
	colorhelp=rep(0,length(labelpred))
	if ( no.modules==0 | no.modules >length(colorcode)){ 
		print(paste("The number of modules is problematic! \n Number of modules = ", as.character(no.modules)))
	} else { 
		for (i in c(1:no.modules)) {
			colorhelp=ifelse(labelpred==modulename[i],colorcode[i],colorhelp)
		}
	}
	names(colorhelp)<-dendro$label
	return(colorhelp)
}


viewModule<-function(object,main="Hierarchical dendrogram and module colors",...){
	#an list object from the output of detectModule
	#with two slot,named as dendrogram and module
	
	if(!is.list(object)){
		stop("object should be a list")
	}
	
	if(is.na(charmatch("module",names(object))) || is.na(charmatch("dendrogram",names(object)))){
		stop("object must have two slot named as 'module' and 'dendrogram'");
	}
	dendro=object$dendrogram
	moduleLables=object$module
	
	if(class(dendro)!="hclust"){
		stop("Error: dendro must be a class of hclust")
	}

	if(length(dendro$order) != length(moduleLables)){
		stop("the size of moduleLables does not equal to the object in the dendro object")
	}
	
	if(!require(moduleColor)){
		stop("package moduleColor is needed")
	}
	
	#assign each module to a certain color
	color=labels2colors(moduleLables)
	
	par(mfrow=c(2,1))
	par(cex = 1.2);
	#mar: c(bottom, left, top, right)
	par(mar = c(0,4.5,2,1));
	plot(dendro,labels=F,main=main, sub="", xlab="")
	par(mar = c(1,4.5,0,1));
	
	#invoke it by moduleColor      
	plotHclustColors(dendro,color,rowLabels="")
}	


#####################################
#save the object to a file
saveModule<-function(object,filename=NULL,sep="\t",iswriteout=TRUE){
	if(!is.list(object)){
		stop("object should be a list")
	}
	
	if(is.na(charmatch("module",names(object))) || is.na(charmatch("dendrogram",names(object)))){
		stop("object must have two slot named as 'module' and 'dendrogram'");
	}
	dendro=object$dendrogram
	moduleLables=object$module
	
	if(class(dendro)!="hclust"){
		stop("Error: dendro must be a class of hclust")
	}
	
	
	if(is.null(filename)){
		filename=paste("module-",format(Sys.time(), "%Y-%m-%d"),".txt",sep="")
	}
	if(!require(moduleColor)){
		stop("package moduleColor is needed")
	}
	
	newlabel=c()
	newnames=c()
	order=dendro$order
	originalnames<-dendro$labels
	for(i in 1:length(moduleLables)){newlabel[i]=moduleLables[order[i]];}
	for(i in 1:length(moduleLables)){newnames[i]=originalnames[order[i]];}	
	datout<-data.frame(module=newlabel,color=labels2colors(newlabel),genes=newnames)
	if(iswriteout){
		write.table(datout,file=filename,row.names=FALSE,sep=sep)	
	}
	return(datout)
}	


######################################################
#
annoModule<-function(object,dofilter=5,dolayer=5,docutoff=0.01,gocutoff=0.01,keggcutoff=0.01,goontologoy="BP",calculateMeanSim=TRUE){
	goontologoys=c("BP","CC","MF")
	if(is.na(match(goontologoy,goontologoys))){
		goontologoy="BP"
	}
	
	if(!is.list(object)){
		stop("object should be a list")
	}
	
	if(is.na(charmatch("module",names(object))) || is.na(charmatch("dendrogram",names(object)))){
		stop("object must have two slot named as 'module' and 'dendrogram'");
	}
	module.data.frame<-saveModule(object,iswriteout=FALSE)
	module.names<-sort(unique(module.data.frame$module)[unique(module.data.frame$module)!=0]) #exclude the module labeled 0
	
	if(length(module.names)==0){
		stop("NO module detect")
	}
	
	
	if(!exists("DOSimEnv")) initialize()	
	
	
	result<-list();
	if(!require("GOSim")){
		stop("package GOSim is needed")
	}
	
	if(!require("SubpathwayMiner")){
		stop("package SubpathwayMiner is needed")
	}
	setOntology(goontologoy)
	#domap<-get("domap",envir=DOSimEnv)
	gomap <- get("gomap",env=GOSimEnv)
	allgenes<-names(gomap)
	
	
	#ALL GENE's similarity list
	simmatrix<-object$simmatrix;
	allsim<-simmatrix[upper.tri(simmatrix)]
	
	if(is.null(names(simmatrix))){
		allgenenamelist<-rownames(simmatrix)
	}else{
		allgenenamelist<-names(simmatrix)
	}
	
	for(i in 1:length(module.names)){
		subgenes<-module.data.frame$genes[module.data.frame$module==module.names[i]]
		modulecolor<-as.character(unique(module.data.frame$color[module.data.frame$module==module.names[i]])[1])
		doresult<-DOEnrichment(subgenes,filter=dofilter,cutoff=docutoff,layer=dolayer)
		goresult<-GOenrichment(subgenes,allgenes,cutoff=1)
		keggresult<-getAnn(subgenes)
		#filter those q-value larger than cutoff for keggresult
		keggindex=rep(FALSE,length(keggresult))
		for(j in 1:length(keggresult)){
			keggitem=keggresult[[j]]
			if(keggitem$qvalue<keggcutoff){
				keggindex[j]=TRUE
			}
		}
		
		keggresult<-keggresult[keggindex]
		
		
		doresult.data.frame<-data.frame("ID"=doresult$DOID,"Name"=doresult$Term,"pvalue"=doresult$pvalue,"qvalue"=doresult$qvalue)
		#######################
		#for goresult
		gopvalue<-sort(goresult$p.values)
		if(length(gopvalue)>0){
			goterm<-goresult$GOTerms
			qvalueList<-p.adjust(gopvalue,method="fdr")
			goindex<-match(names(gopvalue),goterm$go_id)
			subindex<-qvalueList<=gocutoff
			if(sum(subindex)==0){
				goresult.data.frame<-data.frame("ID"=character(),"Name"=character(),"pvalue"=numeric(),"qvalue"=numeric())
			}else{
				goresult.data.frame<-data.frame("ID"=(goterm$go_id[goindex])[subindex],"Name"=(goterm$Term[goindex])[subindex],"pvalue"=gopvalue[subindex],"qvalue"=qvalueList[subindex])
			}
			
			#goresult.data.frame<-data.frame("ID"=goterm$go_id[goindex],"Name"=goterm$Term[goindex],"pvalue"=gopvalue,"qvalue"=qvalueList)
		}else{
			goresult.data.frame<-data.frame("ID"=character(),"Name"=character(),"pvalue"=numeric(),"qvalue"=numeric())
		}	
		
		#######################
		#for keggresult
		if(length(keggresult)>0){
			keggresult.data.frame<-data.frame("ID"=names(keggresult),"Name"=sapply(keggresult,function(a) a$pathwayName),"pvalue"=sapply(keggresult,function(a) a$pvalue),"qvalue"=sapply(keggresult,function(a) a$qvalue))
		}else{
			keggresult.data.frame<-data.frame("ID"=character(),"Name"=character(),"pvalue"=numeric(),"qvalue"=numeric())
		}
		
		
		#####################################
		#calculate the mean similarity of the module, and a t-test to all similarities
		if(calculateMeanSim){
			if(length(allgenenamelist)>0){
				tmpindex=match(subgenes,allgenenamelist)
				submatrix=simmatrix[tmpindex,tmpindex]
				subsimlist=submatrix[upper.tri(submatrix)]
				meansim=mean(submatrix)
				pvalue=t.test(submatrix,allsim,method="greater")$p.value
				sublist=list("DO"=doresult.data.frame,"GO"=goresult.data.frame,"KEGG"=keggresult.data.frame,"Genes"=subgenes,"Size"=length(subgenes),"ModuleColor"=modulecolor,"MeanSimilarity"=meansim,"pvalue"=pvalue)
			}else{
				sublist=list("DO"=doresult.data.frame,"GO"=goresult.data.frame,"KEGG"=keggresult.data.frame,"Genes"=subgenes,"Size"=length(subgenes),"ModuleColor"=modulecolor)
			}
		}else{
			sublist=list("DO"=doresult.data.frame,"GO"=goresult.data.frame,"KEGG"=keggresult.data.frame,"Genes"=subgenes,"Size"=length(subgenes),"ModuleColor"=modulecolor)
		}
		submodulename=paste("module_",module.names[i],sep="")
		result[[submodulename]]<-sublist		
			
	}
	
	return (result)
}


saveAnnoModule<-function(annotatedModule,filename=NULL,sep=","){
	#annotatedModule
	if(!is.list(annotatedModule)){
		stop("annotatedModule is not a list object, use function annoModule to get the result")
	}
	
	if(is.na(charmatch("DO",names(annotatedModule[[1]]))) || 
	   is.na(charmatch("GO",names(annotatedModule[[1]]))) ||
	   is.na(charmatch("KEGG",names(annotatedModule[[1]]))) ||
	   is.na(charmatch("Genes",names(annotatedModule[[1]]))) ||
	   is.na(charmatch("Size",names(annotatedModule[[1]]))) ){
	 	stop("annotatedModule is not a right list object, use function annoModule to get the result")
	 }
	
	
	if(is.null(filename)){
		filename=paste("module-annotation-",format(Sys.time(), "%Y-%m-%d"),".txt",sep="")
	}
	
	fhanlde<-file(filename,"w");
	flag=0;
	if(!is.na(charmatch("pvalue",names(annotatedModule[[1]])))){
		flag=1;
		head=paste("","","","","",'"Disease Ontology"',"","","",'"Gene Ontology"',"","","",'KEGG',"","","","Genes\n",sep=sep)
		cat(head,file=fhanlde)
		head=paste('"Module Name"','"Module Color"','"Module Size"','Mean similarity','pvalue',"ID","Term","pvalue","qvalue","ID","Term","pvalue","qvalue","ID","Term","pvalue","qvalue","\n",sep=sep)
		cat(head,file=fhanlde)
	}else{
		head=paste("","","",'"Disease Ontology"',"","","",'"Gene Ontology"',"","","",'KEGG',"","","","Genes\n",sep=sep)
		cat(head,file=fhanlde)
		head=paste('"Module Name"','"Module Color"','"Module Size"',"ID","Term","pvalue","qvalue","ID","Term","pvalue","qvalue","ID","Term","pvalue","qvalue","\n",sep=sep)
		cat(head,file=fhanlde)
	}
	
	
	for(i in 1:length(annotatedModule)){
		maxlength=max(length(annotatedModule[[i]]$DO$ID),length(annotatedModule[[i]]$GO$ID),length(annotatedModule[[i]]$KEGG$ID),
				length(annotatedModule[[i]]$Genes),1)
		goframe=annotatedModule[[i]]$GO
		doframe=annotatedModule[[i]]$DO		
		keggframe=annotatedModule[[i]]$KEGG
		genelist=annotatedModule[[i]]$Genes
		for(j in 1:maxlength){
			if(j==1){
				if(flag){
					head=paste(names(annotatedModule)[i],annotatedModule[[i]]$ModuleColor,annotatedModule[[i]]$Size,annotatedModule[[i]]$MeanSimilarity,annotatedModule[[i]]$pvalue,sep=sep)
				}else{
					head=paste(names(annotatedModule)[i],annotatedModule[[i]]$ModuleColor,annotatedModule[[i]]$Size,sep=sep)
				}
				
			}else{
				if(flag){
					head=paste("","","","","",sep=sep)
				}else{
					head=paste("","","",sep=sep)
				}
			}
			
			if(j<=length(doframe$ID)){
				head=paste(head,doframe[j,1],doframe[j,2],doframe[j,3],doframe[j,4],sep=sep)
			}else{
				head=paste(head,"","","","",sep=sep)
			}
			
			if(j<=length(goframe$ID)){
				head=paste(head,goframe[j,1],goframe[j,2],goframe[j,3],goframe[j,4],sep=sep)
			}else{
				head=paste(head,"","","","",sep=sep)
			}
			
			if(j<=length(keggframe$ID)){
				head=paste(head,keggframe[j,1],keggframe[j,2],keggframe[j,3],keggframe[j,4],sep=sep)
			}else{
				head=paste(head,"","","","",sep=sep)
			}
			
			if(j<=length(genelist)){
				head=paste(head,genelist[j],sep=sep)
			}else{
				head=paste(head,"",sep=sep)
			}
			
			head=paste(head,"\n",sep="");
			
			cat(head,file=fhanlde)

		}
		cat("\n",file=fhanlde)
	}
		
	close(fhanlde)
}
	
	

