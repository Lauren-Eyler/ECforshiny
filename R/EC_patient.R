EC_patient<-function(Pts, Medoids, Pop, PopClusters, keep_dist=FALSE){
  ptmed<-matrix(nrow=2,ncol=ncol(Pts))
  mednames<-Medoids[,1]
  Medoids<-Medoids[,-1]
  dizzy<-matrix(nrow=nrow(Pts),ncol=nrow(Medoids))
  for (i in 1:nrow(Pts)){
    for (a in 1:nrow(Medoids)){
      ptmed<-rbind(Pts[i,], Medoids[a,])
      dizzy[i,a]<-cluster::daisy(ptmed, metric="gower")
      rm(ptmed)
    }
  }
  colnames(dizzy)<-mednames
  group<-list()
  for (i in 1:nrow(Pts)){
    group[[i]]<-colnames(dizzy)[which(dizzy[i,] == min(dizzy[i,]))]
  }
  for (i in 1:nrow(Pts)){
    if (length(group[[i]])>1){
      group[[i]]<-NA
    }
  }
  group<-unlist(group)
  group<-as.factor(group)
  results<-list()
  results$group<-group
  if (keep_dist==TRUE){
    results$dist<-dizzy
  } else {}
  PatientsNAClustwhich<-which(is.na(results$group)==TRUE)
  PatientsNAClust<-Pts[PatientsNAClustwhich,]
  ptmed<-matrix(nrow=2,ncol=ncol(PatientsNAClust))
  dizzyNA<-matrix(nrow=nrow(PatientsNAClust),ncol=nrow(Pop))
  for (i in 1:nrow(PatientsNAClust)){
    for (a in 1:nrow(Pop)){
      ptmed<-rbind(PatientsNAClust[i,], Pop[a,])
      dizzyNA[i,a]<-cluster::daisy(ptmed, metric="gower")
      rm(ptmed)
    }
  }
  mindist<-list()
  for (i in 1:nrow(PatientsNAClust)){
    mindist[[i]]<-which(dizzyNA[i,]==min(dizzyNA[i,]))
  }
  clustmatch<-list()
  for (i in 1:nrow(PatientsNAClust)){
    clustmatch[[i]]<-PopClusters[mindist[[i]]]
  }
  cmatch_same<-vector()
  for (i in 1:length(clustmatch)){
    if((min(clustmatch[[i]])==max(clustmatch[[i]]))==TRUE){
      cmatch_same[i]=TRUE
    } else {
      cmatch_same[i]=FALSE
    }
  }
  clustmatchone<-rep(NA, nrow(PatientsNAClust))
  for(i in 1:length(clustmatchone)){
    if(cmatch_same[i]==TRUE){
      clustmatchone[i]<-min(clustmatch[[i]])
    }
  }
  clustmatchone<-as.data.frame(clustmatchone)
  rownames(clustmatchone)<-rownames(PatientsNAClust)
  results$group<-as.data.frame(results$group)
  results$group[which((rownames(results$group)%in%rownames(clustmatchone))==TRUE),]<-clustmatchone
  return(results)
}
