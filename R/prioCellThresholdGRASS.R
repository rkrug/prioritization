## [[file:~/Documents/Projects/AlienManagementDrakensberg/packages/prioritization/prioritization.org::*prioCellThresholdGRASS][prioCellThresholdGRASS:1]]
##' Prioritization based on thresholds of several layers
##'
##' Threshold prioritization based on different layers. The final priority value
##' for each cell is based on if the cell is in each layer above or
##' below the threshold.
##'
##' For each input layer, a temporary layer is created with the name
##' \code{tmp_input\[i\]} which is the \code{layerWeight\[i\]} if the value of \code{input\[i\]} is below \code{threshold}, otherwise it is 0.
##'
##' These temporary layers will not be deleted if \code{keepTemporaryLayers} is TRUE.
##'
##' In a final step, all temporarty layers \code{tmp_input\[i\]} will be added up and saved in \code{output}.
##'
##' @usage prioCellThresholdGRASS(input, output, layerWeights,
##' threshold, weight, keepTmpLayers = FALSE, overwrite = FALSE)
##' @name prioCellThresholdGRASS
##' @title Prioritization based on thresholds of several layers
##' 
##' @param input name of layer in GRASScontaining input values -
##' \bold{has to be the same length as \code{layerWeights}}
##' @param output name of output layers containing the priority values 
##' @param layerWeights weights for each \code{input} layer -
##' \bold{has to be the same length as \code{input}}
##' @param threshold value above which layer will be prioritized
##' @param weight final weight of the resulting prioritization.  All
##' values will be multiplied by this value
##' @param keepTmpLayers if \code{TRUE}, temporal layers will be kept
##' @param overwrite if \code{TRUE}, the output layer will be overwritten
##' 
##' @return invisibly returns output layer name
##' 
##' @author Dr Rainer M Krug \email{Rainer@@krugs.de}
##' @export
prioCellThresholdGRASS <- function(
    input,
    output,
    layerWeights,
    threshold,
    weight,
    keepTmpLayers = FALSE,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) ) & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
    op <- options()
    options(warn=-1)
    execGRASS(
        "g.remove",
        rast = output
        )
    options(op)
    names(layerWeights) <- input
    ols <- sapply(
        layerWeights,
        function(l) {
            il <- names(l)
            ol <- paste("tmp", il, sep="_")
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
            return(ol)
        }
        )
    execGRASS(
        "r.mapcalc",
        expression = paste(
            output,
            "=",
            "(", paste(ols, collapse = " + "), ") * weight)" ,
            ),
        flags = "overwrite"
        )
    if (!keepTmpLayers) {
        execGRASS(
            "g.remove",
            rast=paste(ols, collapse=",")
            )
    }
    invisible(output)
}
## prioCellThresholdGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
