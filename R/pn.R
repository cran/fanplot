pn <-
function(sims, p=1:50, anchor=NULL, ...){
  if(min(p)<0 | max(p)>100)
    stop("all percentiles must be between 0 and 100")
  p<-c(p,100-p)
  p<-unique(sort(p))
	pp<-apply(sims,2,quantile,p/100)
	pp<-t(pp)
	if(!is.null(anchor)){
		pp<-rbind(rep(anchor,length(p)),pp)
	}
	pp<-ts(pp, ...)
	class(pp)<-"pn"
	return(pp)
}
