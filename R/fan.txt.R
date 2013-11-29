fan.txt <-
function(psims, pn.r=NULL, pn.l=NULL, pos=NULL, offset=NULL, cex=0.8, hl.lab=NULL,...){
	if(class(psims)!="pn")
		stop("psims must be a pn object (use fan.pn)")
  if(is.null(pn.r) & is.null(pn.l))
    return()
	p<-colnames(psims)
	if(!is.null(hl.lab)){
	  if(length(hl.lab)!=2)
	    stop("hl.lab must be a of length 2 (low/lower/bottom labels first)")
	  plab<-p
	  plab<-gsub("Low",hl.lab[1],plab)
	  plab<-gsub("High",hl.lab[2],plab)
	  colnames(psims)<-plab
	}
  p<-gsub("%","",p)
	if(length(grep("Low",p))>0)
	  type<-"interval"
	if(length(grep("Low",p))==0)
	  type<-"percentile"
	if(type=="interval"){
	  p<-gsub("High","",p)
	  p<-gsub("Low","",p)
	  p.int<-as.numeric(p)
	  p <- c(p.int + (100-p.int)/2, 100 - p.int - (100-p.int)/2)
	  p <- unique(sort(p))
	}
  if(type=="interval"){
    if(!is.null(pn.r)){
      if(sum(pn.r %in% p.int==FALSE)>0)
         print("some text labels (in pn.r) do not have required prediction interval calculated in psims")
      pn.r <- pn.r[pn.r %in% p.int]
      pn.r <- c(pn.r + (100-pn.r)/2, 100 - pn.r - (100-pn.r)/2)
      pn.r <- unique(sort(pn.r))
    }
    if(!is.null(pn.l)){
      if(sum(pn.l %in% p.int==FALSE)>0)
         print("some text labels (in pn.l) do not have required prediction interval calculated in psims")
      pn.l <- pn.l[pn.l %in% p.int]
      pn.l <- c(pn.l + (100-pn.l)/2, 100 - pn.l - (100-pn.l)/2)
      pn.l <- unique(sort(pn.l))
    }
	}
	if(!is.null(pn.r))
     if(any(pn.r %in% p==FALSE))
       print("some text labels (in pn.r) do not have required percentiles calculated in psims")
	if(!is.null(pn.l))
    if(any(pn.l %in% p==FALSE))
      print("some text labels (in pn.l) do not have required percentiles calculated in psims")
	if(is.null(pos) & !is.null(pn.r))	pos<-4
	if(is.null(pos) & !is.null(pn.l))	pos<-2
	if(is.null(offset))	offset<-0.1
  if(type=="percentile"){
    if(!is.null(pn.r))	text(end(psims)[1], psims[dim(psims)[1],match(pn.r,p)], paste0(pn.r, "%"), pos=pos, offset=offset, cex=cex, ...)
    if(!is.null(pn.l))	text(start(psims)[1], psims[1,match(pn.l,p)], paste0(pn.l, "%"), pos=pos, offset=offset, cex=cex, ...)
  }
	if(type=="interval" & is.null(hl.lab)){
	  if(!is.null(pn.r))  text(end(psims)[1], psims[dim(psims)[1],match(pn.r,p)], paste0(p.int[match(pn.r,p)], "%"), pos=pos, offset=offset, cex=cex, ...)
	  if(!is.null(pn.l))	text(start(psims)[1], psims[1,match(pn.l,p)], paste0(p.int[match(pn.l,p)], "%"), pos=pos, offset=offset, cex=cex, ...)
	}
	if(type=="interval" & !is.null(hl.lab)){
	  if(!is.null(pn.r))  text(end(psims)[1], psims[dim(psims)[1],match(pn.r,p)], names(psims[dim(psims)[1],match(pn.r,p)]), pos=pos, offset=offset, cex=cex, ...)
	  if(!is.null(pn.l))	text(start(psims)[1], psims[1,match(pn.l,p)], names(psims[1,match(pn.l,p)]), pos=pos, offset=offset, cex=cex, ...)
	}
}
