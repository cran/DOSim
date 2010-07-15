\name{DOEnrichment}
\alias{DOEnrichment}
\title{DO enrichment analysis}
\description{
	This function performs DO enrichment analysis using Hypergeometric Test or Fisher's Exact Test.
}
\usage{
	DOEnrichment(genelist, method = "hypertest", filter = 5, cutoff = 0.05,adjustp="fdr")
}

\arguments{
  \item{genelist}{
	character vector of Entrez Gene IDs
}
  \item{method}{
	one of ("hypertest","fisher")
}
  \item{filter}{
	indicates that DO terms must have at least 'filter' genes annotated
}
  \item{cutoff}{
	significant cutoff for DO enrichment analysis
}
  \item{adjustp}{
 	multiple test correction with different measures. parameters can be
 	 "holm","hochberg","hommel","bonferroni","BH","BY", or "fdr" [1-9].
}
}
\details{
	Currently the following methods for DO enrichment are implemented: 
	\describe{
\item{"hypertest"}{Using Hypergeometric Test}
\item{"fisher"}{Using Fisher's Exact Test}
}
}
\value{
	Return a data.frame object with 7 columns.Details are below:
	\describe{
\item{"DOID"}{enriched DO ID name}
\item{"Term"}{enriched DO Term name}
\item{"genenum1"}{Gene number annotated to this DO term among whole human genes}
\item{"genenum2"}{Gene number annotated to this DO term in the inputed gene list}
\item{"odds"}{Calculated by \eqn{\frac{m/n}{M/N}}{(m/n)/(M/N)} where '\eqn{m}' stands for the gene number covered by DO in the list,'\eqn{n}' for inputed gene list number,'\eqn{M}' for gene number covered by DO among whole human genes,'\eqn{N}' stands for the gene number of whole human beings.}
\item{"pvalue"}{correspdonding pvalue of enriched DO term}
\item{"adjustedP"}{multiple test correction value for pvalue}

}	
}
\references{ 
[1] Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B, 57, 289-300.

[2] Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29, 1165-1188.

[3] Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65-70.

[4] Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika, 75, 383-386.

[5] Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800-803.

[6] Shaffer, J. P. (1995). Multiple hypothesis testing. Annual Review of Psychology, 46, 561-576. (An excellent review of the area.)

[7] Sarkar, S. (1998). Some probability inequalities for ordered MTP2 random variables: a proof of Simes conjecture. Annals of Statistics, 26, 494-504.

[8] Sarkar, S., and Chang, C. K. (1997). Simes' method for multiple hypothesis testing with positively dependent test statistics. Journal of the American Statistical Association, 92, 1601-1608.

[9] Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics, 48, 1005-1013. (Explains the adjusted P-value approach.)
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
