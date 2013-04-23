head.pn <-
function(x, p=NULL, ...){
  x<-x[1:nrow(x),1:ncol(x)]
  xx<-head(x, ...)
  if(!is.null(p)){
    if(min(p)<0 | max(p)>100)
      stop("all percentiles must be between 0 and 100")
    p<-c(p,100-p)
    p<-unique(sort(p))
    pp<-colnames(xx)
    pp<-gsub("%","",pp)
    pp<-as.numeric(pp)
    if(any(p %in% pp==FALSE))
      stop("all p must be percentiles existing in x")
    xx<-xx[,match(p,pp)]
  }
  xx
}
