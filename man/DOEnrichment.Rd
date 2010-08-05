\name{DOEnrichment}
\alias{DOEnrichment}
\title{DO enrichment analysis}
\description{
	This function performs DO enrichment analysis using Hypergeometric Test.
}
\usage{
	DOEnrichment(genelist,filter=5,cutoff=0.05,layer=5,backgroud=getDefaultBackground())
}

\arguments{
  \item{genelist}{
	character vector of Entrez Gene IDs
}
  \item{filter}{
	indicates that DO terms must have at least 'filter' genes annotated
}
  \item{cutoff}{
	significant cutoff for DO enrichment analysis
}
  \item{layer}{
 	Control for DO terms, this means only those beneath the certain depth defined by users are
 	considered for the analysis.
}
  \item{backgroud}{
	A character vector of Entrez Gene IDs used as backgroud. Default is to use all the genes annotated in DO
}
}
\details{
	The methodology is Hypergeometric Test.
}
\value{
	Return a data.frame object with 9 columns.Details are below:
	
\item{"DOID"}{enriched DO ID name}
\item{"Term"}{enriched DO Term name}
\item{"annGeneNumber"}{Gene number annotated to this DO term in the inputed gene list}
\item{"annBgNumber"}{Gene number in the inputed gene list}
\item{"geneNumber"}{Gene number annotated to this DO term in the backgroud list}
\item{"bgNumber"}{Gene number in the backgroud list}
\item{"odds"}{Calculated by \eqn{\frac{annGeneNumber/annBgNumber}{geneNumber/bgNumber}}{(annGeneNumber/annBgNumber)/(geneNumber/bgNumber)} }
\item{"pvalue"}{siginicance of the hypergeometric test for this DO term}
\item{"qvale"}{multiple test correction value for pvalue using FDR}
	
}

\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##############################
#Examples

  genelist=as.character(1:100)
  res<-DOEnrichment(genelist,filter=50)
  print(res)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
