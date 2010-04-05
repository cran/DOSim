\name{getParents}
\alias{getParents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get direct parents for each DO term
}
\description{
	Returns a list of all direct parents associated to each DO term.
}
\usage{
getParents(dolist, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dolist}{
	character vector of DO IDs 
}
  \item{verbose}{
	print out some information
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List with entry names for each DO ID. Each entry contains a character vector with the direct parent of DO IDs. 
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

\seealso{ \code{\link{getOffsprings}}, \code{\link{getChildren}}, \code{\link{getAncestors}} }
\examples{
################################
#Example

	terms<-c("DOID:934","DOID:1579")
	res<-getParents(terms)
	print(res)
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
