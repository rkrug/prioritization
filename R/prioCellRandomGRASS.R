## [[file:~/Documents/Projects/AlienManagementDrakensberg/packages/prioritization/prioritization.org::*prioCellRandomGRASS][prioCellRandomGRASS:1]]
##' Random prioritization of raster cells
##'
##' This function assignes randomly each cell a priority, i.e. a
##' value, between 0 and 1.
##'
##' This function is a wrapper aroung the function
##' \code{r.surf.random} with \code{MASK} used as the GRASS MASK for
##' the calculations.
##'
##' The function \bold{does not} respect the GRASS MASK as it uses the
##' parameter \code{MASK} as the MASK for the calculations.
##'
##' @usage prioCellRandomGRASS( output, MASK = NULL, overwrite = FALSE )
##' @name prioCellRandomGRASS
##' @title Random prioritization of raster cells
##' 
##' @param output name of output layer with prioritization
##' (0..1)
##' @param MASK name of the GRASS raster layer which will be used as
##' MASK. If it is \code{NULL}, the default, the existing MASK will be
##' used.
##' @param overwrite if \code{TRUE}, the output layer will be overwritten
##'
##' @return invisible outputLayerName
##' @author Dr Rainer M Krug \email{Rainer@@krugs.de}
prioCellRandomGRASS <- function(
    output,
    MASK = NULL,
    overwrite = FALSE
    ) {
    if (!is.null(MASK)) {
        execGRASS(
            "g.rename",
            rast = paste("MASK", "MASK.BAK", sep=",")
            )
        execGRASS(
            "g.rename",
            rast = paste(MASK, "MASK", sep=",")
            )
    }
    try(
        {
            if (overwrite) {
                flags <- "overwrite"
            } else {
                flags <- NULL
            }
            execGRASS(
                "r.surf.random",
                output = output,
                min = 0,
                max = 1,
                flags = flags
                )
        }
        )
    if (!is.null(MASK)) {
        execGRASS(
            "g.rename",
            rast = paste("MASK", MASK, sep=",")
            )
         execGRASS(
            "g.rename",
            rast = paste("MASK.BAK", "MASK", sep=",")
            )
    }
    invisible(output)
}
## prioCellRandomGRASS:1 ends here
