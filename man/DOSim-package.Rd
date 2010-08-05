\name{DOSim-package}
\alias{DOSim-package}
\alias{DOSim}
\docType{package}
\title{DOSim package}
\description{
This package implements multiple similarity measures for DO terms and gene products.
	     It is aiming at disease analyis for gene sets. Modules of a gene set could be detected 
	     and further multilayer annoated on DO, GO and KEGG. We also provides users to conduct DO enrichment
	     analysis and basic information fetching for DO.
}
\details{
\tabular{ll}{
Package: \tab DOSim\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2010-8-5\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jiang Li

Maintainer: Jiang Li \email{riverlee2008@gmail.com}

}


\keyword{ package }
\examples{
#################################
#example
terms<-c("DOID:1579","DOID:945")
tsim<-getTermSim(terms)

print(tsim)
}
