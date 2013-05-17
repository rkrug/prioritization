## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/prioritization/prioritization.org::*prioCellAHPSingle][prioCellAHPSingle:1]]
##' Prioritization based on rules from AHP
##'
##' Prioritization based on an integer input layer, reclass rules and a final weight
##' @title 
##' @param inputLayerName name of layer to be prioritized
##' @param outputLayerName name of output layer containing the priority values 
##' @param grassReclassRules reclass rules as described in the r.reclass help (GRASS)
##'                          The values have to be integer values!
##' @param oneEquals value specifying which of the integer values will equal to one.
##'                  All values will be divided by this value.
##' @param weight final weight of the resulting prioritization.
##'               All values will be multiplied by this value
##' @return invisible outputLayerName
##' @author Dr Rainer M Krug
prioCellAHPSingle <- function(
    inputLayerName,
    outputLayerName,
    grassReclassRules,
    grassRecodeRules,
    oneEquals = 1000,
    weight
    ) {
    if (!missing(grassReclassRules) & !missing(grassRecodeRules)) {
        stop("Only 'grassReclassRules' or 'grassRecodeRules' can be specified!")
    }
    if (missing(grassReclassRules) & missing(grassRecodeRules)) {
        stop("One of 'grassReclassRules' or 'grassRecodeRules' has to be specified specified!")
    }
    op <- options()
    options(warn=-1)
    execGRASS(
        "g.remove",
        rast = outputLayerName
        )
    options(op)
    if (!missing(grassReclassRules)) {
        execGRASS(
            "r.reclass",
            input = inputLayerName,
            output = outputLayerName,
            rules = "-",
            flags = "overwrite",
            Sys_input = grassReclassRules
            )
    } else {
        execGRASS(
            "r.recode",
            input = inputLayerName,
            output = outputLayerName,
            rules = "-",
            flags = "overwrite",
            Sys_input = grassRecodeRules
            )
        oneEquals <- 1
    }   
    execGRASS(
        "r.mapcalc",
        expression = paste(outputLayerName, "=", "float(", outputLayerName, " / ", oneEquals * weight, ")") ,
        flags = "overwrite"
        )
    invisible(outputLayerName)
}
## prioCellAHPSingle:1 ends here
