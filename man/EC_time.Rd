\name{EC_time}
\alias{EC_time}
\title{Time Required to Run N Variable Combinations on N Cores
}
\description{Running the 'EconomicClusters' algorithm takes significant computing time. 'EC_time' determines the amount of time needed to run 'EconomicClusters' for one combination of variables on each available core in parallel. These results can be used to estimate the overall duration of the algorithm for all variable combinations.
}
\usage{EC_time(X, Y, nvars, kmin, kmax, ncores)}
\arguments{
  \item{X}{ a data frame with Column 1 containing weighted number of household members (coded as numeric) and Columns 2 through n containing all asset variables to be considered for variable selection (coded as factors). A data frame in this form can be produced by function 'EC_DHSwts' for DHS data. Column names should be specified.}
  \item{Y}{ optional variable in vector form to be included in all variable combinations. See details below.}
  \item{nvars}{ number of asset variables used to define economic clusters. If Y is not missing, the number of asset variables used to define economic clusters will be nvars+1.}
  \item{kmin}{ minimum number of clusters to be considered}
  \item{kmax}{ maximum number of clusters to be considered}
  \item{ncores}{ number of computing cores to be used in parallel}
}
\value{
  Time needed to run 'EconomicClusters' on n combinations of variables in parallel using n computing cores
}
\note{
Running the full 'EconomicClusters' algorithm on a real household survey data set will take a significant amount of computing time. Don't worry! We have a free and publically available solution for you. Please see the help files for 'EconomicClusters-package' for further details.
}

\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters-package}}, \code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_DHSwts}}, \code{\link[EconomicClusters:EconomicClusters-package]{data_for_EC}}
}

\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}




\keyword{function}
