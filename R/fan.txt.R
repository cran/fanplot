fan.txt <-
function(psims, pn.r=NULL, pn.l=NULL, pos=NULL, offset=NULL, cex=0.8, ...){
  if(class(psims)!="pn")
		stop("psims must be a pn object (use fan.pn)")
  if(is.null(pn.r) & is.null(pn.l))
    return()
	p<-colnames(psims)
  p<-gsub("%","",p)
	if(!is.null(pn.r))
     if(any(pn.r %in% p==FALSE))
       print("some text labels (in pn.r) do not have required percentiles calculated in psims")
	if(!is.null(pn.l))
    if(any(pn.l %in% p==FALSE))
      print("some text labels (in pn.l) do not have required percentiles calculated in psims")
	if(is.null(pos) & !is.null(pn.r))	pos<-4
	if(is.null(pos) & !is.null(pn.l))	pos<-2
	if(is.null(offset))	offset<-0.1
  if(!is.null(pn.r))	text(end(psims)[1], psims[dim(psims)[1],match(pn.r,p)], paste0(pn.r, "%"), pos=pos, offset=offset, cex=cex, ...)
  if(!is.null(pn.l))	text(start(psims)[1], psims[1,match(pn.l,p)], paste0(pn.l, "%"), pos=pos, offset=offset, cex=cex, ...)
}
