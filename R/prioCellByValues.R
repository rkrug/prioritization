## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/prioritization/prioritization.org::*prioCellByValues][prioCellByValues:1]]
##' Prioritization based on years
##'
##' Prioritization based on values specified and layer containing values. If cell contains a value listed in "values"
##' the priority value of this cell will be prioValues[1], otherwise prioValues[2]
##' 
##' @title 
##' @param yearLayerName name of layer containing years
##' @param outputLayerName name of output layer containing the priority values 
##' @param values values which should get priority value prioValues[1]
##' @param prioValues priority values, first one for in years, second one not
##' @param oneEquals value specifying which integer values will equal to one.
##'                  All values will be divided by this value.
##' @param weight final weight of the resulting prioritization.
##'               All values will be multiplied by this value
##' @return invisible outputLayerName
##' @author Dr Rainer M Krug
prioCellByValues <- function(
  yearLayerName,
  outputLayerName,
  values,
  prioValues,
  oneEquals = 100, 
  weight
  ) {
  op <- options()
  options(warn=-1)
  execGRASS(
    "g.remove",
    rast = outputLayerName
    )
  options(op)
  rule <- paste( paste( paste(years, collapse=" "), prioValues[1], sep = " = "), "* = ", prioValues[2], sep="\n" )
  execGRASS(
    "r.reclass",
    input = yearLayerName,
    output = outputLayerName,
    rules = "-",
    Sys_input = rule,
    flags = "overwrite"
    )
  execGRASS(
    "r.mapcalc",
    expression = paste(outputLayerName, "=", "float(", outputLayerName, " / ", oneEquals * weight, ")") ,
    flags = "overwrite"
    )
  invisible(outputLayerName)
}
## prioCellByValues:1 ends here
