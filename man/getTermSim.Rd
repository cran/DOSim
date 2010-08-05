\name{getTermSim}
\alias{getTermSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get pairwise DO term similarities.
}
\description{
	Returns the pairwise similarities between DO terms based on different methods.
}
\usage{
getTermSim(termlist, method = "relevance", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{termlist}{
	character vector of DO terms
}
  \item{method}{
	one ot the supported methods for DO term similairty(see below)
}
  \item{verbose}{
	print out various information or not
}
}
\details{
Currently the following methods for computing DO term similarities are implemented: 
  \describe{
\item{"Resnik"}{information content of minimum subsumer (ICms) [1]}
\item{"JiangConrath"}{\eqn{1-\min(1,IC(term1)-2ICms+IC(term2))}{1-min(1,IC(term1)-2ICms+IC(term2))}[2]}           
\item{"Lin"}{\eqn{\frac{2ICms}{(IC(term1) + IC(term2))}}{2ICms/(IC(term1)+IC(term2))} [3]}
\item{"CoutoResnik"}{average information content of common disjunctive ancestors of \code{term1} and \code{term2} (ICshare) [4]}
\item{"CoutoJiangConrath"}{\eqn{1 - \min(1, IC(term1) -2ICshare + IC(term2))}{1-min(1,IC(term1)-2ICshare+IC(term2))} [4]}
\item{"CoutoLin"}{\eqn{\frac{2ICshare}{(IC(term1) + IC(term2))}}{2ICshare/(IC(term1) + IC(term2))} [4]}
\item{"relevance"}{sim_Lin * (1 - exp(-ICms)) [5]}
\item{"GIC"}{summed information content of common ancestors divided by summed information content of all ancestors of term1 and term2 [7]}
\item{"simIC"}{sim_Lin * (1 - 1/(1 + ICms)) [7]}
\item{"Wang"}{\eqn{Sim(term1,term2)=\frac{\sum_{t\in T_{term1}\cap T_{term2}} (S_{term1}(t)+S_{term2}(t))}{SV(term1)+SV(term2)}} [8]}
}
}
\value{
n x n matrix (n = number of DO terms) with similarities between DO terms. 
}

\references{
[1] P. Resnik, Using Information Content to evaluate semantic similarity in a taxonomy, Proc. 14th Int. Conf. Artificial Intel., 1995 

[2] J. Jiang, D. Conrath, Semantic Similarity based on Corpus Statistics and Lexical Taxonomy, Proc. Int. Conf. Research in Comp. Ling., 1998 

[3] D. Lin, An Information-Theoretic Definition of Similarity, Proc. 15th Int. Conf. Machine Learning, 1998 

[4] Couto, F.; Silva, M. & Coutinho, P., Semantic Similarity over the Gene Ontology: Family Correlation and Selecting Disjunctive Ancestors, Conference in Information and Knowledge Management, 2005  

[5] A. Schlicker, F. Domingues, J. Rahnenfuehrer, T. Lengauer, A new measure for functional similarity of gene products based on Gene Ontology, BMC Bioinformatics, 7, 302, 2006.

[6] C. Pesquita, D. Faria, H. Bastos, A. Falcao, F. Couto, Evaluating GO-based Semantic Similarity Measures, In: Proc. 10th Annual Bio-Ontologies Meeting 2007, 37 - 40, 2007

[7] B. Li, J. Wang, A. Feltus, J. Zhou, F. Luo, Effectively Integrating Information Content and Structural Relationship to Improve the GO-based Similarity Measure Between Proteins, BMC Bioinformatics, 2009. 

[8] James Z.Wang,Zhidian Du, et al. A new method to measure the semantic similarity of GO terms.Bioinformatics 2007,Vol 23,1274-1281.
}
\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}
\note{
All calculations use normalized information contents for each DO
  term. Normalization is achieved by dividing each information content
  by the maximum information content.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{getMinimumSubsumer}},
  \code{\link{getDisjCommAnc}}} 
\examples{
################################
#Example
	getTermSim(c("DOID:8","DO:1117"),method="Lin")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
