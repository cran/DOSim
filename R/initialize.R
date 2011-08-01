.First.lib<-function(lib, pkgname){
  #library.dynam(pkgname, pkgname, lib)
  initialize_DOSimEnv()
}


initialize_DOSimEnv <-
function(){
	#print("initializing DOSim package ...")		
	data("DOSimEnv")
	#load("DOSimEnv.rda");
	#assign("DOSimEnv",DOSimEnv,envir=.GlobalEnv)  	
  	#print("finished.")
}

