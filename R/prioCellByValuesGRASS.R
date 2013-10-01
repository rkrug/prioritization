## [[file:~/Documents/Projects/AlienManagementDrakensberg/packages/prioritization/prioritization.org::*prioCellByValuesGRASS][prioCellByValuesGRASS:1]]
##' Prioritization based on specific values
##'
##' Prioritization based on actual values specified and layer
##' containing values. If a cell contains a value listed in
##' \code{values}, the priority value of this cell will be
##' \code{prioValues\[1\]}, otherwise code{prioValues\[2\]}
##'
##' @usage prioCellByValuesGRASS(input, output, values, prioValues, oneEquals = 100,weight, overwrite=FALSE)
##' @name prioCellByValuesGRASS
##' @title Prioritization based on specific values
##' 
##' @param input name of layer containing input values
##' @param output name of output layer containing the priority values 
##' @param values values which should get priority value prioValues[1]
##' @param prioValues priority values, first one for in \code{values}, second one not
##' @param oneEquals value specifying which integer values will equal
##' to one.  All values will be divided by this value.
##' @param weight final weight of the resulting prioritization.  All
##' values will be multiplied by this value
##' @param overwrite if \code{TRUE}, output layer will be overwritten
##'
##' @return invisible output
##' @author Dr Rainer M Krug \email{Rainer@@krugs.de}
##' @export
prioCellByValuesGRASS <- function(
    input,
    output,
    values,
    prioValues,
    oneEquals = 100, 
    weight,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) ) & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
   rule <- paste(
       paste(
           paste(values, collapse=" "),
           prioValues[1], sep = " = "),
       "* = ", prioValues[2], sep="\n" )
    execGRASS(
        "r.reclass",
        input = input,
        output = output,
        rules = "-",
        Sys_input = rule,
        flags = "overwrite"
        )
    execGRASS(
        "r.mapcalc",
        expression = paste(output, "=", "float(", output, " / ", oneEquals * weight, ")") ,
        flags = "overwrite"
        )
    invisible(output)
}
## prioCellByValuesGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
