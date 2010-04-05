\name{getDOGraph}
\alias{getDOGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get DO graph with specified DO terms at its leave.
}
\description{
	The function getDOGraph returns a graphNEL object representing the DO graph with leaves specified in the argument.
}
\usage{
getDOGraph(term, prune = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
	character vector of DO term IDs
}
  \item{prune}{
	do not show the complete graph, but prune it after the specified number of ancestors
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
graphNEL object(s)
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
if(require(graph)){
	g<-getDOGraph(c("DOID:95","DOID:8"))
	if(require(Rgraphviz)){ 
		plot(g)
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
