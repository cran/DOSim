\name{getShortestPath}
\alias{getShortestPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the shortest path between two terms
}
\description{
	Get the shortest path between two terms.
}
\usage{
getShortestPath(term1, term2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term1}{
	DO term 1
}
  \item{term2}{
	DO term 2 
}
}

\value{
return the shortest path between two terms, if two term are not connect, the return value is Inf
}

\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
######################################
#example
	term1="DOID:8"
	term2="DOID:5"
	getShortestPath(term1,term2)
	
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
