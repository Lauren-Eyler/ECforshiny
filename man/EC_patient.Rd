\name{EC_patient}
\alias{EC_patient}
\title{Assign Patients to Economic Clusters
}
\description{Function 'EC_patient' assigns patients in your study or registry to an economic cluster, as defined by the 'EconomicClusters' algorithm, based on the similarity of patients' assets to the assets of the cluster medoids.
}
\usage{EC_patient(Pts, Medoids, keep_dist)}

\arguments{
  \item{Pts}{ a data frame with columns consisting of patients' responses to the economic variables that define your economic model, with column names specified}
  \item{Medoids}{ a data frame with columns consisting of cluster medoids' responses to the economic variables that define your economic model, with column names specified (as produced by function 'EconomicClusters')}
  \item{keep_dist}{TRUE or FALSE indicating whether to keep the matrix of distances from patients to cluster medoids. See details.}
}
\details{'EC_patient' determines the distance between each patient and each cluster medoid using function 'daisy' from package 'cluster' with metric = "gower". See 'daisy' help files for further details. In the context of clustering algorithms, "distance" refers to how dissimilar two observations are based on their responses to a set of variables. 'EC_patient' assigns each patient to a cluster by the smallest distance from that patient to a cluster medoid.

If a patient is equidistant from the medoids of two or more clusters, 'EC_patient' will return NA. If this occurs, set keep_dist to TRUE to determine which clusters the patient is closest to. Otherwise, you can set keep_dist to FALSE to avoid storing the large distance matrix.
}
\value{ A vector of cluster membership (denoted by the row index of the cluster medoid) for each patient}

\seealso{
\code{\link[EconomicClusters:EconomicClusters-package]{EconomicClusters}}, \code{\link[EconomicClusters:EconomicClusters-package]{Pts}}, \code{\link[EconomicClusters:EconomicClusters-package]{Medoids}}
}
\author{Lauren Eyler
  \email{economic.clusters@gmail.com}
}

\keyword{function}
