EC_patient<-function(Pts, Medoids, keep_dist=FALSE){
  ptmed<-matrix(nrow=2,ncol=ncol(Pts))
  dizzy<-matrix(nrow=nrow(Pts),ncol=nrow(Medoids))
  for (i in 1:nrow(Pts)){
    for (a in 1:nrow(Medoids)){
      ptmed<-rbind(Pts[i,], Medoids[a,])
      dizzy[i,a]<-cluster::daisy(ptmed, metric="gower")
      rm(ptmed)
    }
  }
  group<-list()
  for (i in 1:nrow(Pts)){
    group[[i]]<-which(dizzy[i,] == min(dizzy[i,])) 
  }
  for (i in 1:nrow(Pts)){
    if (length(group[[i]])>1){
    group[[i]]<-NA
    }
  }
  group<-unlist(group)
  results<-list()
  results$group<-group
  if (keep_dist==TRUE){
    results$dist<-dizzy
  } else {}
  return(results)
#' @export
}
