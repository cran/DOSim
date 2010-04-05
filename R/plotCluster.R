plotCluster <-
function(hier,h=0.9,minsize=5,main="Cluster Dendrogram", ...){
if(class(hier) != "hclust")
stop("Error: hier must be a class of hclust")

modulecolor<-function(hier, h=0.9,minsize=5) {
# here we define modules by using a height cut-off for the branches
if(!exists("DOSimEnv")) initialize()
labelpred= cutree(hier,h=h)
sort1=-sort(-table(labelpred))
modulename= as.numeric(names(sort1))
modulebranch= sort1>minsize
no.modules=sum(modulebranch)
# now we assume that there are fewer than a certain number of colors
colorcode=get("GlobalStandardColors",envir=DOSimEnv)
# "grey" means not in any module;
colorhelp=rep("grey",length(labelpred))
if ( no.modules==0 | no.modules >length(colorcode)){ print(paste("The number of modules is problematic! \n Number of modules = ", as.character(no.modules)))} else { for (i in c(1:no.modules)) {colorhelp=ifelse(labelpred==modulename[i],colorcode[i],colorhelp)};
colorhelp=factor(colorhelp,levels=c(colorcode[1:no.modules],"grey"))
}
factor(colorhelp, levels=unique(colorhelp[hier$order] ))
}

couleur=as.character(modulecolor(hier,h=h, minsize=minsize))
par(mfrow=c(2,1),mar=c(0,2,2,1))
plot(hier, main=main, labels=F, xlab="", sub="",...);
if (length(hier$order) != length(couleur) ) {
stop("ERROR: length of color vector not compatible with no. of objects in the hierarchical tree")
}
if (length(hier$order) == length(couleur) ) {
barplot(height=rep(1, length(couleur)), col= as.character(couleur[hier$order]),border=F,space=0, axes=F,...)
}
}

