\name{getDisjCommAnc}
\alias{getDisjCommAnc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get disjoint common ancestors.
}
\description{
 Returns the DO terms representing the disjoint common ancestors of two DO terms. 
}
\usage{
getDisjCommAnc(term1, term2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term1}{ DO term 1 }
  \item{term2}{ DO term 2 }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Character vector of DO terms
}
\references{ Couto, F.; Silva, M. & Coutinho, P., Semantic Similarity over the Gene Ontology: Family Correlation and Selecting Disjunctive Ancestors, Conference in Information and Knowledge Management, 2005 }

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

	if(require(graph))	
	getDisjCommAnc("DOID:934","DOID:95")
	
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
