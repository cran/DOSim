\name{getGeneSim}
\alias{getGeneSim}
	
\title{
Compute disease similarity for genes base on DO
}
\description{
Calculate the pairwise disease similarities for a list of genes using different strategies
}
\usage{
getGeneSim(genelist, similarity="funSimMax", similarityTerm="relevance", normalization=TRUE, method="sqrt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genelist}{ character vector of Entrez gene IDs }
  \item{similarity}{ method to calculate the disease similarity between gene products }
  \item{similarityTerm}{ method to compute the similarity of DO terms }
  \item{normalization}{ normalize similarities yes/no}
  \item{method}{"sqrt": normalize sim(x,y) <- sim(x,y)/sqrt(sim(x,x)*sim(y,y)); "Lin": normalize sim(x,y) <- 2*sim(x,y)/(sim(x,x) + sim(y,y)); "Tanimoto": normalize sim(x,y) <- sim(x,y)/(sim(x,x) + sim(y,y) - sim(x,y)).}
  \item{verbose}{ print out some information }
}
\details{
   The method to calculate the pairwise disease similarity between
   gene products can either be: 
   \describe{
\item{"max"}{the maximum similarity between any two DO terms}
\item{"mean"}{the average similarity between any two DO terms1[1]}
\item{funSimMax}{ the average of best matching DO term similarities. Take the maximum of the scores achieved by assignments of DO terms from gene 1 to gene 2 and vice versa. [2]}
\item{funSimAvg}{ the average of best matching DO term similarities. Take the average of the scores achieved by assignments of DO terms from gene 1 to gene 2 and vice versa. [2]}
\item{"BMA"}{best match average approach [3]} 
}
}
\value{
  n x n similarity matrix (n = number of genes)
}
\references{ 
[1] P. W. Lord, et al., "Investigating semantic similarity measures across the Gene Ontology: the relationship between sequence and annotation," Bioinformatics, vol. 19, pp. 1275-83, Jul 1 2003.

[2] A. Schlicker, F. Domingues, J. Rahnenfuehrer, T. Lengauer, A new measure for functional similarity of gene products based on Gene Ontology, BMC Bioinformatics, 7, 302, 2006.

[3] James Z.Wang,Zhidian Du, et al. A new method to measure the semantic similarity of GO terms.Bioinformatics 2007,Vol 23,1274-1281.

} 

\seealso{  \code{\link{getTermSim}}}
\examples{
####################################
#Example
	genelist=1:10
	gsim<-getGeneSim(genelist)
	print(gsim)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
