getWeightedDotSim <-
function(anno1, anno2){
	v1 = getGeneFeatures.internal(anno1)
	v2 = getGeneFeatures.internal(anno2)
	dot = crossprod(v1,v2)	
}

