print.ECshiny<-function(x, ...){
  cat("Results of Economic Clustering\n")
  cat("Max ASW:\n")
  cat(paste(x[[1]]),"\n")
  cat("Assets:\n")
  cat(paste(x[[2]]),"\n")
  cat("Number of Clusters:\n")
  cat(paste(x[[3]]),"\n")
  cat("Cluster Medoids:\n")
  cat(paste(x[[4]]),"\n")
  cat("\n")
  cat("A data frame containing only cluster medoids with only clustering variables will be emailed to you.\n")
  cat("\n")
  cat("A vector of cluster membership for all data points will be emailed to you. \n")
#' @export
}
