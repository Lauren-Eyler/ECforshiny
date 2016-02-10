EC_DHSwts<-function(X, dejure, defacto, HHwt){
  for (i in 1:length(dejure)) {
    if(dejure[i] ==0) {
      dejure[i] = defacto[i]
    } 
  }
  Weights = (dejure * HHwt) / 1000000
  X<-cbind(Weights,X)
  return(X)
#' @export
}



