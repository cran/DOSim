DOGraph <-
function(term, env){
    oldEdges <- vector("list", length = 0)
    oldNodes <- vector("character", length = 0)
    newN <- term
    done <- FALSE
    while (!done) {
        newN <- newN[!(newN %in% oldNodes)]
        if (length(newN) == 0)
            done <- TRUE
        else {
            oldNodes <- c(oldNodes, newN)
            numE <- length(newN)
            #nedges <- AnnotationDbi::mget(newN, env = env, ifnotfound = NA)
            nedges <- mget(newN, env=env,ifnotfound= NA)
            nedges <- nedges[!is.na(nedges)]
            oldEdges <- c(oldEdges, nedges)
            if (length(nedges) > 0)
                newN <- sort(unique(unlist(nedges)))
            else newN <- NULL
        }
    }
    rE <- vector("list", length = length(oldNodes))
    names(rE) <- oldNodes
    rE[names(oldEdges)] <- oldEdges
    rE <- lapply(rE, function(x) match(x, oldNodes))
    names(oldNodes) = oldNodes
    return(new("graphNEL", nodes = oldNodes, edgeL = lapply(rE,
        function(x) list(edges = x)), edgemode = "directed"))
}

