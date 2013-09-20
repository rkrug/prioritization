\name{prioCellAHPSingleGRASS}
\alias{prioCellAHPSingleGRASS}
\title{Prioritization based on rules from AHP}
\usage{
  prioCellAHPSingleGRASS(input, output, grassReclassRules,
    grassRecodeRules, oneEquals = 1000, weight, overwrite =
    FALSE)
}
\arguments{
  \item{input}{name of input layer to be used as the basis
  of the AHP prioritization}

  \item{output}{name of output layer containing the
  priority values}

  \item{grassReclassRules}{reclass rules as described in
  the r.reclass help (GRASS). The values have to be integer
  values!}

  \item{grassRecodeRules}{recode rules as described in the
  r.recode help (GRASS).}

  \item{oneEquals}{value specifying which of the integer
  values will equal to one.  All values will be divided by
  this value.}

  \item{weight}{final weight of the resulting
  prioritization.  All values will be multiplied by this
  value}

  \item{overwrite}{if \code{TRUE}, existing output ayer
  will be overwritte.}
}
\value{
  invisible rwturns name of output layer
}
\description{
  Prioritization based on rules from AHP
}
\details{
  Prioritization based on an integer input layer, reclass
  rules and a final weight.  Either the
  \code{grassReclassRules} or the \code{grassRecodeRules}
  have to be specified!
}
\author{
  Dr Rainer M Krug \email{Rainer@krugs.de}
}

