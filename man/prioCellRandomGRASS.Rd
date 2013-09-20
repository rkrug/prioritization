\name{prioCellRandomGRASS}
\alias{prioCellRandomGRASS}
\title{Random prioritization of raster cells}
\usage{
  prioCellRandomGRASS( output, MASK = NULL, overwrite =
    FALSE )
}
\arguments{
  \item{output}{name of output layer with prioritization
  (0..1)}

  \item{MASK}{name of the GRASS raster layer which will be
  used as MASK. If it is \code{NULL}, the default, the
  existing MASK will be used.}

  \item{overwrite}{if \code{TRUE}, the output layer will be
  overwritten}
}
\value{
  invisible outputLayerName
}
\description{
  Random prioritization of raster cells
}
\details{
  This function assignes randomly each cell a priority,
  i.e. a value, between 0 and 1.

  This function is a wrapper aroung the function
  \code{r.surf.random} with \code{MASK} used as the GRASS
  MASK for the calculations.

  The function \bold{does not} respect the GRASS MASK as it
  uses the parameter \code{MASK} as the MASK for the
  calculations.
}
\author{
  Dr Rainer M Krug \email{Rainer@krugs.de}
}

