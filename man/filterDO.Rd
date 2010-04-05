\name{filterDO}
\alias{filterDO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Filter DO}
\description{
Filter out genes from a list not mapping to the disease ontology.
}
\usage{
filterDO(genelist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genelist}{
 	character vector of Entrez gene IDs
}
}
\details{
Filter out genes from a list not mapping to the disease ontology,and return a list which the genes are in the disease ontology.
}
\value{
List with items
   \item{"genename"}{gene ID}
   \item{"annotation"}{character vector of DO IDs mapping to the gene}
}

\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
################################
#Example
	genelist<-1:10
	res<-filterDO(genelist)
	print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
