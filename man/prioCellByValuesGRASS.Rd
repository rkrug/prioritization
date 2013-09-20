\name{prioCellByValuesGRASS}
\alias{prioCellByValuesGRASS}
\title{Prioritization based on specific values}
\usage{
  prioCellByValuesGRASS(input, output, values, prioValues,
    oneEquals = 100,weight, overwrite=FALSE)
}
\arguments{
  \item{input}{name of layer containing input values}

  \item{output}{name of output layer containing the
  priority values}

  \item{values}{values which should get priority value
  prioValues[1]}

  \item{prioValues}{priority values, first one for in
  \code{values}, second one not}

  \item{oneEquals}{value specifying which integer values
  will equal to one.  All values will be divided by this
  value.}

  \item{weight}{final weight of the resulting
  prioritization.  All values will be multiplied by this
  value}

  \item{overwrite}{if \code{TRUE}, output layer will be
  overwritten}
}
\value{
  invisible output
}
\description{
  Prioritization based on specific values
}
\details{
  Prioritization based on actual values specified and layer
  containing values. If a cell contains a value listed in
  \code{values}, the priority value of this cell will be
  \code{prioValues\[1\]}, otherwise code{prioValues\[2\]}
}
\author{
  Dr Rainer M Krug \email{Rainer@krugs.de}
}

