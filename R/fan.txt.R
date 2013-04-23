fan.txt <-
function(psims, pn.r=NULL, pn.l=NULL, pos=NULL, offset=NULL, cex=0.8, ...){
	if(class(psims)!="pn")
		stop("psims must be a pn object (use fan.pn)")
	p<-colnames(psims)
	p<-gsub("%","",p)
	p<-as.numeric(p)
	if(!is.null(pn.r))
		if(any(pn.r %in% p==F))
			stop("all pn.r must be percentiles existing in psims")
	if(!is.null(pn.l))
		if(any(pn.l %in% p==F))
			stop("all pn.l must be percentiles existing in psims")
	if(is.null(pos) & !is.null(pn.r))	pos<-4
	if(is.null(pos) & !is.null(pn.l))	pos<-2
	if(is.null(offset))	offset<-0.1
	if(!is.null(pn.r))	text(end(psims)[1], psims[dim(psims)[1],match(pn.r,p)], paste0(pn.r, "%"), pos=pos, offset=offset, cex=cex, ...)
	if(!is.null(pn.l))	text(start(psims)[1], psims[1,match(pn.l,p)], paste0(pn.l, "%"), pos=pos, offset=offset, cex=cex, ...)
}
