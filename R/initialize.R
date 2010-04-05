initialize <-
function(){
	print("initializing DOSim package ...")		
	data("DOSimEnv")
	#load("DOSimEnv.rda");
	assign("DOSimEnv",DOSimEnv,envir=.GlobalEnv)  	
  	print("finished.")
}

