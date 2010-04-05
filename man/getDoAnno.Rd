\name{getDoAnno}
\alias{getDoAnno}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get gene list associated to each DO term
}
\description{
Get gene list associated to each DO term
}
\usage{
getDoAnno(dolist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dolist}{
 character vector of DO IDs
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List with entry names for each DO ID. Each entry contains a character vector with associated Entrez Gene IDs.
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
\code{\link{getDoTerm}}
}
\examples{
################################
#Example

	terms<-c("DOID:934","DOID:1579")
	res<-getDoAnno(terms)
	print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
