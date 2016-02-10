\name{ECforshiny-package}
\alias{ECforshiny-package}
\docType{package}
\title{Population-Specific Economic Groups Defined by Few Asset Variables}
\description{The aim of this package is to facilitate health disparities research in time-constrained, low-resource settings, such as trauma registries in low- and middle-income countries. The 'ECforshiny' algorithm defines population-specific metrics of economic status based on limited numbers of asset variables. This package is designed for use with large-scale household survey data, such as Demographic and Health Survey (DHS) data sets. Economic groups are defined by running weighted k-medoids clustering using package 'WeightedCluster' on all combinations of a limited number of asset variables. The combination of variables and number of clusters with the highest average sillhouette width (ASW) are selected as the optimal economic clustering model. We also include support functions to facilitate the use of the 'ECforshiny' algorithm, including a function to assign study subjects to an economic cluster. For a more detailed description of this methodology, please see Eyler et al. (2016)[1]. Please note that the 'ECforshiny' algorithm can require significant computing time to run on a real household survey data set. For a free, publically available option for acquiring server time, please see www... (insert tutorial link).}
\details{
\tabular{ll}{
Package: \tab ECforshiny\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2015-10-30\cr
License: \tab GPL (>=3)\cr
}




}
\author{
Lauren Eyler, Alan Hubbard, Catherine Juillard

Maintainer: Lauren Eyler <economic.clusters@gmail.com>
}
\references{
1. Eyler LE, Hubbard A, Juillard CJ...

2.  Rutstein SO. Steps to Constructing the New DHS Wealth Index. ICF International: Rockville, Maryland, \url{http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
}

3. Kaufman L, Rousseeuw PJ. Finding Groups in Data: An Introduction to Cluster Analysis (Vol. 344). John Wiley & Sons: Hoboken, New Jerkey, 2009.
  }

\keyword{ package }
\seealso{\code{\link[WeightedCluster]{wcKMedRange}}, \code{\link[cluster]{daisy}}, \code{\link[foreach]{foreach}}, \code{\link[doParallel]{doParallel}
}}
