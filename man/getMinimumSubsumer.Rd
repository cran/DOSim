\name{getMinimumSubsumer}
\alias{getMinimumSubsumer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute minimum subsumer of two DO terms
}
\description{
Returns the minimum subsumer(i.e. the common ancestor having the
maximal information content) of two DO terms 
}
\usage{
getMinimumSubsumer(term1, term2)
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
\details{
	The result is computed base on current disease ontology
}
\value{
	DO term representing the minmum subsumber. If there is no minumum subsumer,the result is the string "NA".
}
\references{P. Resnik, Using Information Content to evaluate semantic similarity in a taxonomy, Proc. 14th Int. Conf. Artificial Intel., 1995 }

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
###########################
#Example
	term1="DOID:8"
	term2="DOID:95"
	getMinimumSubsumer(term1,term2)
	
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
