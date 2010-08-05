\name{detectModule}
\alias{detectModule}
\title{Detect modules from gene set}
\description{
  Detect modules from the gene set, input is a gene similarity matrix or distance matirx which can be obtained 
  by \link{getGeneSim} function.
}
\usage{
detectModule(dat, isSimilarity=1, hierarchicalMethod="average", cutHeight = NULL, minClusterSize = 10, 
                       method = "hybrid", deepSplit = (ifelse(method=="hybrid", 1, FALSE)), 
                       maxCoreScatter = NULL, minGap = NULL,
                       maxAbsCoreScatter = NULL, minAbsGap = NULL, 
                       pamStage = TRUE, pamRespectsDendro = TRUE,
                       useMedoids = FALSE, maxDistToLabel = cutHeight,
                       respectSmallClusters = TRUE, 
                       verbose = 2, indent = 0)

}
\arguments{
  \item{dat}{A symmetrical matrix object.Values can be either a similarity matrix or distance matrix, which can be obtained   by \link{getGeneSim} function.   }
  \item{isSimilarity}{Is the input data a similarity matrix or distance matrix}
  \item{hierarchicalMethod}{method can be "average","complete",etc., when using \link{hclust} to calculate the dendrogram}
  \item{cutHeight}{Maximum joining heights that will be considered. For \code{method=="tree"} it
defaults to 0.99. For \code{method=="hybrid"} it defaults to 99% of the range between the 5th
percentile and the maximum of the joining heights on the dendrogram.}
  \item{minClusterSize}{Minimum cluster size.  }
  \item{method}{Chooses the method to use. Recognized values are "constant", "hybrid" and "tree".  }
  \item{deepSplit}{For method "hybrid", can be either logical or integer in the range 0 to 4. For method
"tree", must be logical. In both cases, provides a rough control over sensitivity to cluster splitting.
The higher the value (or if \code{TRUE}), the more and smaller clusters will be produced. For the
"hybrid" method, a finer control can be achieved via \code{maxCoreScatter} and \code{minGap} below.}
  \item{maxCoreScatter}{Only used for method "hybrid". 
Maximum scatter of the core for a branch to be a cluster, given as the fraction of \code{cutHeight}
relative to the 5th percentile of joining heights. See Details.  }
  \item{minGap}{Only used for method "hybrid". 
Minimum cluster gap given as the fraction of the difference between \code{cutHeight} and the 5th
percentile of joining heights.  }
  \item{maxAbsCoreScatter}{Only used for method "hybrid". 
Maximum scatter of the core for a branch to be a cluster given as absolute heights. If given, overrides
\code{maxCoreScatter}.  }
  \item{minAbsGap}{Only used for method "hybrid". 
Minimum cluster gap given as absolute height difference. If given, overrides \code{minGap}.  }
  \item{pamStage}{Only used for method "hybrid".  If TRUE, the second (PAM-like) stage will be performed.  }  

  \item{pamRespectsDendro}{Logical, only used for method "hybrid". If \code{TRUE}, the PAM stage will
respect the dendrogram in the sense that objects and small clusters will only be assigned to clusters
that belong to the same branch that the objects or small clusters being assigned belong to.
}    

  \item{useMedoids}{Only used for method "hybrid" and only if \code{labelUnlabeled==TRUE}.
If TRUE, the second stage will be use object to medoid distance; if FALSE, it
  will use average object to cluster distance. The default (FALSE) is recommended. }
  \item{maxDistToLabel}{Only used for method "hybrid" and only if \code{labelUnlabeled==TRUE}.
Maximum object distance to closest cluster that will result in the object
assigned to that cluster.  }
  \item{respectSmallClusters}{Only used for method "hybrid" and only if \code{labelUnlabeled==TRUE}.
If TRUE, branches that failed to be clusters in stage 1 only because of
insufficient size will be assigned together in stage 2. If FALSE, all objects will be assigned
individually.}
  \item{verbose}{Controls the verbosity of the output. 0 will make the function completely quiet,
values up to 4 gradually increase verbosity.}
  \item{indent}{Controls indentation of printed messages (see \code{verbose} above). Each unit
adds two spaces before printed messages; useful when several functions' output is to be nested. }
}
\details{
  This function use the output of \code{getGeneSim}, and further apply hierarchical
clustering method on it, use three branch cutting methods to detect modules. Details for the branch cutting methods
please see \link{cutreeDynamic}[1].

  The input data will be scaled to 0-1 first using Min-Max method, which can be formulated as (x-min(x)/(max(x)-min(x))
  
  The output of this function can be further used as the input of \link{saveModule} and \link{viewModule}
}
\value{
  A vector of numerical labels giving assignment of objects to modules. Unassigned objects are labeled
0, the largest module has label 1, next largest 2 etc.
  a list object contains three slots. They are:
  1) "dendrogram", the dendrogram calculated by \code{hclust} on dat
  
  2) "module",a vector of numerical labels giving assignment of objects to modules. Unassigned objects are labeled
0, the largest module has label 1, next largest 2 etc.

  3) "simmatrix", the similarity matrix of input data after Min-Max processing. If the input dat is a distance matrix, it convert to 
  similarity matrix by 1-Min-Max(dat)
}
\references{
  Langfelder P, Zhang B, Horvath S, 2007.
  \url{http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting}
}
\author{
 Jiang Li<\email{riverlee2008@gmail.com}>
}
\seealso{
  \code{\link{hclust}}, \code{\link{cutreeDynamic}},\code{\link{getGeneSim}}.
}
\examples{
##############################
#Examples

  data(obesity)
  modules<-detectModule(obesity,method="tree",minClusterSize=10)


}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
