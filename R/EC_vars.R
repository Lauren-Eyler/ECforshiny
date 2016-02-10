EC_vars<-function(X, p, catstart){
  assets_num<-data.matrix(X[,1:(catstart-1)])
  for(i in 1:nrow(assets_num)){
    for (a in 1:ncol(assets_num)){
      if (assets_num[i,a]==1){
        assets_num[i,a]<-0
      } else {
        assets_num[i,a]<-1
      }
    }
  }
  common_names<-colnames(assets_num[,colMeans(assets_num, na.rm=TRUE)>p])
  pop<-X[,colnames(X) %in% common_names==TRUE]
  pop<-cbind(pop, X[,catstart:ncol(X)])
  return(pop)
#' @export
}

