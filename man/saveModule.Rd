\name{saveModule}
\alias{saveModule}
\title{Save the module result}
\description{
  Save the module result to files in a user friendly format.
}
\usage{
saveModule(object,filename=NULL,sep="\t",iswriteout=TRUE)

}
\arguments{
 \item{object}{A list object which is the output of \code{detectModule}.   }
 \item{filename}{Output filename, if filename is not set, it will be named as "module-year-month-day.txt" format}
 \item{sep}{separate character used in the output file}
 \item{iswriteout}{indicate whether write the result into file}	
}
\details{
  This function is to organize the module result in a user friendly format, more readable for users.
}
\value{
  Return a data.frame with three columns named as 1) module; 2) color; 3) genes. If parameter iswriteout is set TRUE, it will write the result into a file.
}

\author{
 Jiang Li<\email{riverlee2008@gmail.com}>
}
\seealso{
  \code{\link{detectModule}}, \code{\link{viewModule}}.
}
\examples{
##############################
#Examples
 #
 # obesitysim<-data(obesity)
 # modules<-detectModule(obesitysim,method="tree",minClusterSize=10)
 # saveModule(modules)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
