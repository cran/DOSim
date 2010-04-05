\name{getChildren}
\alias{getChildren}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a list of all direct children of each DO term}
\description{
	Returns the list of all direct children associated to each DO term.
}
\usage{
getChildren(dolist, verbose = TRUE)
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
List with entry names for each DO ID. Each entry contains a character vector with the direct children of DO IDs. 
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

\seealso{ \code{\link{getOffsprings}}, \code{\link{getParents}},
  \code{\link{getAncestors}} }
\examples{
################################
#Example

	terms<-c("DOID:934","DOID:1579")
	res<-getChildren(terms)
	print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
