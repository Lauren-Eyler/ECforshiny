EC_time<-function(X, Y=rep(NA, nrow(X)), nvars, kmin, kmax, ncores){
  start.time<-Sys.time()
  wt<-X[,1]
  X<-X[,-1]
  col_cat<-seq(1,ncol(X),1)
  combn<-utils::combn(c(1:ncol(X)),nvars)
  col_indx=matrix(c("NULL"), ncol(combn), ncol(X), byrow=FALSE)
  for (i in 1:ncol(combn)){
    col_indx[i,]<-t(is.element(col_cat, combn[,i]))
    col_indx[i,][col_indx[i,]==FALSE]<-NA
    colnames(col_indx)=colnames(X)
  }
  doParallel::registerDoParallel(cores=ncores)
  ASW<-matrix(nrow=nrow(col_indx), ncol=(kmax-kmin+1))
#' @import foreach
  ASW<-foreach::foreach (i=1:ncores, .combine='rbind') %dopar% {
    combi<-X[,!is.na(col_indx[i,])]
    if(any(!is.na(Y))){
      combi<-cbind(Y, combi)
    }
    dizzy<-cluster::daisy(combi, metric="gower")
    wcKMR<-WeightedCluster::wcKMedRange(dizzy, kvals=seq(kmin, kmax, 1), weights=wt)
    ASW[i,]<-wcKMR$stats[,5]
    rm(combi)
    rm(dizzy)
    rm(wcKMR)
    return(ASW[i,])
  }
  ASW_max<-max(ASW, na.rm=TRUE)
  ASW_max_model<-which(ASW == ASW_max, arr.ind=TRUE)
  model<-list()
  Assets<-list()
  clust<-list()
  K<-list()
  for (a in 1:nrow(ASW_max_model)){
    model[[a]]<-ASW_max_model[a,1]
    Assets[[a]]<-which( !is.na(col_indx[model[[a]],]), arr.ind=TRUE)
    Assets[[a]]<-names(Assets[[a]])
    if (any(!is.na(Y))){
      Assets[[a]]<-c(deparse(substitute(Y)), Assets[[a]])
    }
    clust[[a]]<-ASW_max_model[a,2]
    K[[a]]<-kmin-1+clust[[a]]
  }
  results<-list()
  results$ASW_max<-ASW_max
  results$Assets<-Assets
  results$K<-K
  end.time<-Sys.time()
  time<-end.time - start.time
  return(time)
#' @export
}
