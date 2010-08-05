\name{viewModule}
\alias{viewModule}
\title{Display modules}
\description{
  Display modules from the output of \code{detectModule}
}
\usage{
viewModule(object,main="Hierarchical dendrogram and module colors",...)

}

\arguments{
  \item{object}{A list object which is the output of \code{detectModule}.   }
  \item{main}{The main title for the graph}
  \item{...}{other parameters set to display the graph}
  
}
\details{
   visualize the modules detect by \code{detectModule} in a graph format.
}

\author{
 Jiang Li<\email{riverlee2008@gmail.com}>
}
\seealso{
  \code{\link{detectModule}}.
}
\examples{
##############################
#Examples

  data(obesity)
  modules<-detectModule(obesity,method="tree",minClusterSize=10)
  viewModule(modules)


}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
