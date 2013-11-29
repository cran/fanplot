pn <-
function(sims, p=1:50, p.int=NULL, anchor=NULL, type="percentile", ...){
  if(min(p)<0 | max(p)>100)
    stop("all percentiles must be between 0 and 100")
  if(!is.null(p.int))
    if(min(p.int)<0 | max(p.int)>100)
      stop("all prediction intervals must be between 0 and 100")
  if(!is.null(p.int)){
    p<-NULL
    type<-"interval"
  }
  if(type=="interval" & is.null(p.int)){
    p.int<-c(50,80,95)
  }
  if(type=="percentile"){
	  p<-c(p,100-p)
	  p<-unique(sort(p))
	}  
  if(type=="interval"){
    p <- c(p.int + (100-p.int)/2, 100 - p.int - (100-p.int)/2)
    p <- unique(sort(p))
  }
	pp<-apply(sims,2,quantile,p/100)
	if(type=="interval")
    rownames(pp)<-paste0(c(rev(p.int),p.int),"%" ,rep(c(" Low", " High"), each=length(p)/2))
	pp<-t(pp)
	if(!is.null(anchor)){
		pp<-rbind(rep(anchor,length(p)),pp)
	}
	pp<-ts(pp, ...)
	class(pp)<-"pn"
	return(pp)
}
