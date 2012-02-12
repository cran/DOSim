\name{annoModule}
\alias{annoModule}
\title{Multilayer annotation for each module}
\description{
 Multilayer annotation for each module from DO, GO, and KEGG, in order to explore the implied the functional meaning.
}
\usage{
annoModule(object,dofilter=5,dolayer=5,docutoff=0.01,
	      gocutoff=0.01,goontologoy="BP",
	      calculateMeanSim=TRUE)
}
\arguments{
   \item{object}{A list object which is the output of \code{detectModule}.   }
   \item{dofilter}{indicate that there must be at least "dofilter" genes annotated on this do term, then it will be considered for analysis}
   \item{dolayer}{indicate that do terms beneath depth "dolayer" in the DAG of DO will be considered for analysis}
   \item{docutoff}{significant filter for do terms}
   \item{gocutoff}{significant filter for go terms}
   \item{goontologoy}{which category of GO is used when conducting analysis for the module result, value is one of "BP","MF",and "CC"}
   \item{calculateMeanSim}{whether to calculate the mean similariy for each module}   
}
\details{
	The methodology for multilaper annotation is the enrichment analysis on DO and GO.
  }
\value{
  return a list object with size equal to the module number. Each item in the list contains all the information for a certain module, and it
  is also a list object contains 6 or 8 slots(is parameter 'calculateMeanSim' is set TRUE).
  They are:
  1) DO, a data.frame object with four columns: ID, Name, pvalue and qvalue;
  2) GO, a data.frame object with four columns: ID, Name, pvalue and qvalue;
  3) Genes, a character vector of genes covered by the module;
  4) Size, the size of the module, which is equal to the number of genes covered by the module;
  5) ModuleColor, color stands for the module;
  6) MeanSimilarity, the average gene similarity for the module (if parameter calculateMeanSim is set TRUE);
  7) pvalue, t-test significant for comparison of gene similarity in the module with gene similarity in the gene list (if parameter calculateMeanSim is set TRUE).
  

}

\author{
 Jiang Li<\email{riverlee2008@gmail.com}>
}
\seealso{
  \code{\link{detectModule}}, \code{\link{DOEnrichment}}.
}
\examples{
##############################
#Examples

  #
  #data(obesity)
  #modules<-detectModule(obesity,method="tree",minClusterSize=10)
  #annoModule(modules,dolayer=NULL)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
