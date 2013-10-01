## [[file:~/Documents/Projects/AlienManagementDrakensberg/packages/prioritization/prioritization.org::*prioCellAHPSingleGRASS][prioCellAHPSingleGRASS:1]]
##' Prioritization based on rules from AHP
##'
##' Prioritization based on an integer input layer, reclass rules and
##' a final weight.  Either the \code{grassReclassRules} or the
##' \code{grassRecodeRules} have to be specified!
##'
##' @usage prioCellAHPSingleGRASS(input, output, grassReclassRules,
##' grassRecodeRules, oneEquals = 1000, weight, overwrite = FALSE)
##' @name prioCellAHPSingleGRASS
##' @title Prioritization based on rules from AHP
##' @param input name of input layer to be used as the basis of the
##' AHP prioritization
##' @param output name of output layer containing the priority values
##' @param grassReclassRules reclass rules as described in the
##' r.reclass help (GRASS). The values have to be integer values!
##' @param grassRecodeRules recode rules as described in the
##' r.recode help (GRASS).
##' @param oneEquals value specifying which of the integer values will
##' equal to one.  All values will be divided by this value.
##' @param weight final weight of the resulting prioritization.  All
##' values will be multiplied by this value
##' @param overwrite if \code{TRUE}, existing output ayer will be overwritte.
##' 
##' @return invisible rwturns name of output layer
##' @author Dr Rainer M Krug \email{Rainer@@krugs.de}
##' @export
prioCellAHPSingleGRASS <- function(
    input,
    output,
    grassReclassRules,
    grassRecodeRules,
    oneEquals = 1000,
    weight,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) ) & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
    if (!missing(grassReclassRules) & !missing(grassRecodeRules)) {
        stop("Only 'grassReclassRules' or 'grassRecodeRules' can be specified!")
    }
    if (missing(grassReclassRules) & missing(grassRecodeRules)) {
        stop("One of 'grassReclassRules' or 'grassRecodeRules' has to be specified specified!")
    }
    execGRASS
    op <- options()
    options(warn=-1)
    execGRASS(
        "g.remove",
        rast = output
        )
    options(op)
    if (!missing(grassReclassRules)) {
        execGRASS(
            "r.reclass",
            input = input,
            output = output,
            rules = "-",
            flags = "overwrite",
            Sys_input = grassReclassRules
            )
    } else {
        execGRASS(
            "r.recode",
            input = input,
            output = output,
            rules = "-",
            flags = "overwrite",
            Sys_input = grassRecodeRules
            )
        oneEquals <- 1
    }   
    execGRASS(
        "r.mapcalc",
        expression = paste(output, "=", "float(", output, " / ", oneEquals * weight, ")") ,
        flags = "overwrite",
        )
    invisible(output)
}
## prioCellAHPSingleGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
