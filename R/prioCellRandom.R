## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/prioritization/prioritization.org::*prioCellRandom][prioCellRandom:1]]
##' Random prioritization of raster cells
##'
##' Cells will be randmly prioritized by assigning a random value [0, 1) for each cell.
##' @title Random prioritization of raster cells
##' @param inputLayerName name of layer for which the prioritization should be done.
##'                       NA:  cell will not be prioritized
##'                       !NA: cell will be prioritized
##' @param outputLayerName name of output layer with prioritization (0..1)
##' 
##' @return invisible outputLayerName
##' @author Dr Rainer M Krug
prioCellRandom <- function(
    inputLayerName,
    outputLayerName) {
    prio <- readRAST6( inputLayer )
    inds <- which(!is.na(prio[[1]]))
    prio[[1]][inds] <- runif(inds)
    ## prio@proj4string <- parameter$proj4string
    writeRAST6(
        prio,
        outputLayer, # layerName(list(layer="all"),  type="prioStand",  parameter$year)
        1,
        overwrite = TRUE
        )
    invisible(outputLayerName)
}
## prioCellRandom:1 ends here
