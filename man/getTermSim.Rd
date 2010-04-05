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
\item{"CoutoEnriched"}{FuSSiMeg enriched term similarity by Couto et al. [4]}.
\item{"CoutoResnik"}{average information content of common disjunctive ancestors of \code{term1} and \code{term2} (ICshare) [5]}
\item{"CoutoJiangConrath"}{\eqn{1 - \min(1, IC(term1) -2ICshare + IC(term2))}{1-min(1,IC(term1)-2ICshare+IC(term2))} [5]}
\item{"CoutoLin"}{\eqn{\frac{2ICshare}{(IC(term1) + IC(term2))}}{2ICshare/(IC(term1) + IC(term2))} [5]}
\item{"relevance"}{sim_Lin * (1 - exp(-ICms)) [6]}
\item{"GIC"}{summed information content of common ancestors divided by summed information content of all ancestors of term1 and term2 [7]}
\item{"simIC"}{sim_Lin * (1 - 1/(1 + ICms)) [8]}
\item{"path"}{\eqn{\frac{1}{p}} where \eqn{p} is the number of nodes on the shortest path [9][11]} 
\item{"lch"}{\eqn{-\log(\frac{p}{2.depth})} where depth is maximum depth of the hierachy  [10][11]}
\item{"Wang"}{\eqn{Sim(term1,term2)=\frac{\sum_{t\in T_{term1}\cap T_{term2}} (S_{term1}(t)+S_{term2}(t))}{SV(term1)+SV(term2)}} [12]}
}
}
\value{
n x n matrix (n = number of DO terms) with similarities between DO terms. 
}

\references{
[1] P. Resnik, Using Information Content to evaluate semantic similarity in a taxonomy, Proc. 14th Int. Conf. Artificial Intel., 1995 

[2] J. Jiang, D. Conrath, Semantic Similarity based on Corpus Statistics and Lexical Taxonomy, Proc. Int. Conf. Research in Comp. Ling., 1998 

[3] D. Lin, An Information-Theoretic Definition of Similarity, Proc. 15th Int. Conf. Machine Learning, 1998 

[4] F. Couto, M. Silva, P. Coutinho, Implementation of a Functional Semantic Similarity Measure between Gene-Products, DI/FCUL TR 03-29, Department of Informatics, University of Lisbon, 2003 

[5] Couto, F.; Silva, M. & Coutinho, P., Semantic Similarity over the Gene Ontology: Family Correlation and Selecting Disjunctive Ancestors, Conference in Information and Knowledge Management, 2005  

[6] A. Schlicker, F. Domingues, J. Rahnenfuehrer, T. Lengauer, A new measure for functional similarity of gene products based on Gene Ontology, BMC Bioinformatics, 7, 302, 2006.

[7] C. Pesquita, D. Faria, H. Bastos, A. Falcao, F. Couto, Evaluating GO-based Semantic Similarity Measures, In: Proc. 10th Annual Bio-Ontologies Meeting 2007, 37 - 40, 2007

[8] B. Li, J. Wang, A. Feltus, J. Zhou, F. Luo, Effectively Integrating Information Content and Structural Relationship to Improve the GO-based Similarity Measure Between Proteins, BMC Bioinformatics, 2009. 

[9] Wu Z, Palmer M. Verb semantics and lexical selection.In:Proceedings of the 32nd annual meeting of the association for computational linguistics. Las Cruces, NM; 1994.p.133-8.

[10] Leacock C,Chodorow M. Combining local context and WordNet similarity for word sense identification. In: Fellbaum C, editor. WordNet: An electronic lexical database. Cambridge, MA: MIT Press;1998.p.265-83.

[11] Pedersen, T., S. V. S. Pakhomov, et al. Measures of semantic similarity and relatedness in the biomedical domain. Journal of Biomedical Informatics 2007 40(3): 288-299.

[12] James Z.Wang,Zhidian Du, et al. A new method to measure the semantic similarity of GO terms.Bioinformatics 2007,Vol 23,1274-1281.
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
