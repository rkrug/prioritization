\name{prioCellThresholdGRASS}
\alias{prioCellThresholdGRASS}
\title{Prioritization based on thresholds of several layers}
\usage{
  prioCellThresholdGRASS(input, output, layerWeights,
    threshold, weight, keepTmpLayers = FALSE, overwrite =
    FALSE)
}
\arguments{
  \item{input}{name of layer in GRASScontaining input
  values - \bold{has to be the same length as
  \code{layerWeights}}}

  \item{output}{name of output layers containing the
  priority values}

  \item{layerWeights}{weights for each \code{input} layer -
  \bold{has to be the same length as \code{input}}}

  \item{threshold}{value above which layer will be
  prioritized}

  \item{weight}{final weight of the resulting
  prioritization.  All values will be multiplied by this
  value}

  \item{keepTmpLayers}{if \code{TRUE}, temporal layers will
  be kept}

  \item{overwrite}{if \code{TRUE}, the output layer will be
  overwritten}
}
\value{
  invisibly returns output layer name
}
\description{
  Prioritization based on thresholds of several layers
}
\details{
  Threshold prioritization based on different layers. The
  final priority value for each cell is based on if the
  cell is in each layer above or below the threshold.

  For each input layer, a temporary layer is created with
  the name \code{tmp_input\[i\]} which is the
  \code{layerWeight\[i\]} if the value of \code{input\[i\]}
  is below \code{threshold}, otherwise it is 0.

  These temporary layers will not be deleted if
  \code{keepTemporaryLayers} is TRUE.

  In a final step, all temporarty layers
  \code{tmp_input\[i\]} will be added up and saved in
  \code{output}.
}
\author{
  Dr Rainer M Krug \email{Rainer@krugs.de}
}

