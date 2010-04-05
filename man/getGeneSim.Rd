\name{getGeneSim}
\alias{getGeneSim}
	
\title{
Compute functional similarity for genes
}
\description{
Calculate the pairwise functional similarities for a list of genes using different strategies
}
\usage{
getGeneSim(genelist, similarity = "funSimMax", similarityTerm = "relevance", normalization = TRUE, method = "sqrt", avg = (similarity == "OA"), verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genelist}{ character vector of Entrez gene IDs }
  \item{similarity}{ method to calculate the functional similarity between gene products }
  \item{similarityTerm}{ method to compute the similarity of DO terms }
  \item{normalization}{ normalize similarities yes/no}
  \item{method}{"sqrt": normalize sim(x,y) <- sim(x,y)/sqrt(sim(x,x)*sim(y,y)); "Lin": normalize sim(x,y) <- 2*sim(x,y)/(sim(x,x) + sim(y,y)); "Tanimoto": normalize sim(x,y) <- sim(x,y)/(sim(x,x) + sim(y,y) - sim(x,y)). NOTE: normalization does not have any effect, if similarity = "funSimMax", "funSimAvg" or similarity = "OA" and avg=TRUE}
  \item{avg}{ standardize the OA kernel by the maximum number of DO terms for both genes}
  \item{verbose}{ print out some information }
}
\details{
   The method to calculate the pairwise functional similarity between
   gene products can either be: 
   \describe{
\item{"max"}{the maximum similarity between any two DO terms}
\item{"mean"}{the average similarity between any two DO terms}
\item{funSimMax}{ the average of best matching DO term similarities. Take the maximum of the scores achieved by assignments of DO terms from gene 1 to gene 2 and vice versa. [2]}
\item{funSimAvg}{ the average of best matching DO term similarities. Take the average of the scores achieved by assignments of DO terms from gene 1 to gene 2 and vice versa. [2]}
\item{"OA"}{the optimal assignment (maximally weighted bipartite
  matching) of DO terms associated to the gene having fewer annotation
  to the DO terms of the other gene. [1]}
\item{"hausdorff"}{the translation of the Hausdorff distance between two sets: Let A and B be the two sets of DO terms associated to two genes. Then \eqn{sim(A,B) = min(min(max_{x \in A}(x,y)), min(max_{y\in B}(x,y)))}. [3]}
\item{"dot"}{the dot product between feature vectors describing the absence/presence of each DO term. The absence of each DO
 term is weighted by its information content. Depending on the type of later normalization one can arrive at the cosine similarity (method="sqrt") or at the Tanimoto coefficient (method="Tanimoto").[4]}
\item{"Wang"}{Assume gene1 have m DO annoated(DO1=\{do1,do2,...,dom\}) and gene2 have n DO annotated(DO2=\{do1,do2,...,don\}).Define Sum1 is the  sum of maximum of the scores achieved by assignments of each DO in DO1 to DO2,same for Sum2, and the Sim(g1,g2)=(Sum1+Sum2)/(m+n) [5]} 
}
}
\value{
  n x n similarity matrix (n = number of genes)
}
\references{ 
[1] H. Froehlich, N. Speer, C. Spieth, A. Zell, Kernel
  Based Functional Gene Grouping, Proc. Int. Joint Conf. on Neural
  Networks (IJCNN), 6886 - 6891, 2006.

[2] A. Schlicker, F. Domingues, J. Rahnenfuehrer, T. Lengauer, A new measure for functional similarity of gene products based on Gene Ontology, BMC Bioinformatics, 7, 302, 2006.

[3] A. del Pozo, F. Pazos, A. Valencia, Defining functional distances over Gene Ontology, BMC Bioinformatics, 9:50, 2008.

[4] M. Mistry, P Pavlidis, Gene Ontology term overlap as a measure of gene functional similarity, BMC Bioinformatics, 9:327, 2008.

[5] James Z.Wang,Zhidian Du, et al. A new method to measure the semantic similarity of GO terms.Bioinformatics 2007,Vol 23,1274-1281.

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
