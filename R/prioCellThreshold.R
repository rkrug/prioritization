## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/prioritization/prioritization.org::*prioCellThreshold][prioCellThreshold:1]]
##' Prioritization based on thresholds of several layers
##'
##' Threshold prioritization based on different layers.
##' Final value for each cell is based on if the cell is in each layer above or below the threshold:
##'   - above, the appropriate value gets added,
##'   - below, nothing gets added
##' 
##' @title 
##' @param inputLayerNames 
##' @param outputLayerName name of output layers containing the priority values 
##' @param layerWeights 
##' @param threshold value above which layer will be prioritized
##' @param weight final weight of the resulting prioritization.
##'               All values will be multiplied by this value
##' 
##' @param keepTmpLayers if TRUE, temporary layers are not deleted
##' @return invisible outputLayerName
##' @author Dr Rainer M Krug
prioCellThreshold <- function(
    inputLayerNames,
    outputLayerName,
    layerWeights,
    threshold,
    weight,
    keepTmpLayers = FALSE
    ) {
    op <- options()
    options(warn=-1)
    execGRASS(
        "g.remove",
        rast = outputLayerName
        )
    options(op)
    names(layerWeights) <- inputLayerNames
    ols <- sapply(
        layerWeights,
        function(l) {
            il <- names(l)
            ol <- paste0("tmp", il)
            execGRASS(
                "r.recode",
                input = il,
                output = ol,
                rules = "-",
                flags = "overwrite",
                Sys_input = c(
                    paste(threshold, "*", l, sep=":"),
                    paste("*", threshold, 0, sep=":")
                    )
                )
            returen(ol)
        }
        )
    execGRASS(
        "r.mapcalc",
        expression = paste(outputLayerName, "=", paste(ols, collapse=" + ") * weight) ,
        flags = "overwrite"
        )
    if (!keepTmpLayers) {
        execGRASS(
            "g.remove",
            rast=paste(ol, collapse=",")
            )
    }
    invisible(outputLayerName)
}
## prioCellThreshold:1 ends here
