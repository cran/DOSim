\name{DOSim-package}
\alias{DOSim-package}
\alias{DOSim}
\docType{package}
\title{DOSim package}
\description{
This package implements several functions useful for computing
	similarities between DO terms and gene products based on their DO annotation.
	Moreover it allows for computing a DO enrichment analysis and provide basic
	disease ontology annotation.
}
\details{
\tabular{ll}{
Package: \tab DOSim\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-04-05\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jiang Li

Maintainer: Jiang Li <\email{riverlee2008@gmail.com}>

}
\references{}

\keyword{ package }
\examples{
#################################
#example
terms<-c("DOID:1579","DOID:945")
tsim<-getTermSim(terms)

print(tsim)
}
