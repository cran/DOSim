\name{plotCluster}
\alias{plotCluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plot a cluster Dendrogram
}
\description{
	Plot a cluster Dendrogram 
}
\usage{
plotCluster(hier, h = 0.9, minsize = 5, main = "Cluster Dendrogram", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hier}{
	an hclust object
}
  \item{h}{
	height cut-off for the branches
}
  \item{minsize}{
	cluster size cut-off 
}
  \item{main}{
	plot's name
}
\item{...}{
}	other parameters
}

\value{
	show a picture with top part a cluster dendrogram and below is a barplot which the same color indicates a module defined by argument "h" and "minsize"
}

\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
	require(graphics)
	hc <- hclust(dist(USArrests), "ave")
	plotCluster(hc,h=50,minsize=5,hang=-1)
	
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
