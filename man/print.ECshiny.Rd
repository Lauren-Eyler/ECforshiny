\name{print.ECshiny}
\alias{print.ECshiny}
\title{Print Output of 'EconomicClusters' Algorithm
}
\description{
  Prints output of the 'EconomicClusters' algorithm in a user-friendly manner.
}
\usage{
\method{print}{ECshiny}(x, ...)
}
\arguments{
  \item{x}{ an object of class 'ECshiny', as produced by function 'ECforshiny'}
  \item{\dots}{ additional arguments to \code{\link{print}}
  }
}
\details{ This function is called internally by 'ECforshiny'.
}
\value{
  A printout of maximum ASW, variables selected, cluster number, and medoid row indices
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}
\seealso{\code{\link{ECforshiny}}
}

