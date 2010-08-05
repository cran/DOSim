\name{saveAnnoModule}
\alias{saveAnnoModule}
\title{Detect modules from gene set}
\description{
  Detect modules from the gene set, input is a gene similarity matrix or distance matirx which can be obtained 
  by \link{getGeneSim} function.
}
\usage{
saveAnnoModule(annotatedModule,filename=NULL,sep=",")

}
\arguments{
	\item{annotatedModule}{A list object which is the output of \code{annoModule}}
	\item{filename}{Output filename, if filename is not set, it will be named as "module-annotation-year-month-day.txt" format}
 	\item{sep}{separate character used in the output file}

}



\author{
 Jiang Li<\email{riverlee2008@gmail.com}>
}
\seealso{
  \code{\link{detectModule}}, \code{\link{annoModule}}.
}
\examples{
##############################
#Examples

  #data(obesity)
  #modules<-detectModule(obesity,method="tree",minClusterSize=10)
  #anno<-annoModule(modules)
  #saveAnnoModule(anno)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
