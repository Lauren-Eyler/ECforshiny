\name{EC_DHSwts}
\alias{EC_DHSwts}
\title{Generate Vector of Weighted Number of Household Members
}
\description{In order to use a nationally-representative population for cluster analysis, 'EconomicClusters' uses weighted k-medoids clustering from the package 'WeightedCluster'. If employed for analysis of Demographic and Health Surveys (DHS) data, we recommend using the weights used by the DHS to calculate the wealth index[1]. 'EC_DHSwts' generates a vector of weighted number of household members using methods described in the DHS protocol[1].
}
\usage{EC_DHSwts(X, dejure, defacto, HHwt)}
\arguments{
  \item{X}{ a data frame of all asset variables to be considered for variable selection coded as factors with column names specified}
  \item{dejure}{ the variable specified by the DHS to describe number of dejure household members (see the DHS recode manual for your country for details)}
  \item{defacto}{ the variable specified by the DHS to describe number of defacto household members (see the DHS recode manual for your country for details)}
  \item{HHwt}{DHS household weight variable (see the DHS recode manual for your country for details)}
}
\details{As per DHS protocols for generation of the wealth index[1], 'EC_DHSwts' multiplies the household sampling weight by the number of dejure household members (or the number of defacto household members if number of dejure members = 0) and divides the result by 1,000,000.}
\value{
  A data frame with Column 1 containing weighted number of household members and Columns 2 through n containing all asset variables to be considered for variable selection
}
\references{
1. Rutstein SO. Steps to Constructing the New DHS Wealth Index. ICF International: Rockville, Maryland, \url{http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
  }
}
\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EC_vars}}, \code{\link[EconomicClusters:EconomicClusters-package]{EC_time}}, \code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}, \code{\link[EconomicClusters:EconomicClusters-package]{HH_survey}}, \code{\link[EconomicClusters:EconomicClusters-package]{assets}}
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}
\note{'EconomicClusters' can be used with other nationally representative household-level surveys that ask about assets. Weights appropriate to the given survey should be applied.
}

\keyword{function}
