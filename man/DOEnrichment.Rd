\name{DOEnrichment}
\alias{DOEnrichment}
\title{DO enrichment analysis}
\description{
	This function performs a DO enrichment analysis using Hypergeometric Test or Fisher's Exact Test.
}
\usage{
	DOEnrichment(genelist, method = "hypertest", filter = 5, cutoff = 0.05)
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
}
\details{
	Currently the following methods for DO enrichment are implemented: 
	\describe{
\item{"hypertest"}{Using Hypergeometric Test}
\item{"fisher"}{Using Fisher's Exact Test}
}
}
\value{
	Return a data.frame object with 6 columns.Details are below:
	\describe{
\item{"DOID"}{enriched DO ID name}
\item{"pvalue"}{correspdonding pvalue of enriched DO term}
\item{"odds"}{Calculated by \eqn{\frac{m/n}{M/N}}{(m/n)/(M/N)} where '\eqn{m}' stands for the gene number covered by DO in the list,'\eqn{n}' for inputed gene list number,'\eqn{M}' for gene number covered by DO among whole human genes,'\eqn{N}' stands for the gene number of whole human beings.}
\item{"genenum1"}{Gene number annotated to this DO term among whole human genes}
\item{"genenum2"}{Gene number annotated to this DO term in the inputed gene list}
}	
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
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
