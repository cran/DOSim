\name{getAncestors}
\alias{getAncestors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a list of all ancestors associated to each DO term }
\description{
	Returns the list of all ancestors associated to each DO term.
}
\usage{
getAncestors(dolist, verbose = TRUE)
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

\value{
	List with entry names for each DO ID. Each entry contains a charcter vector with the ancestor DO IDs
}

\author{
Jiang Li<\email{riverlee2008@gmail.com}>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{getOffsprings}}, \code{\link{getChildren}}, \code{\link{getParents}} }
\examples{
################################
#Example

	terms<-c("DOID:934","DOID:1579")
	res<-getAncestors(terms)
	print(res)


	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
