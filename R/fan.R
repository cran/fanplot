fan <-
function(psims, fan.col=heat.colors(floor(dim(psims)[2]/2)), ln.col=fan.col[1],
	ln=seq(10,90,10), txt=ln, hl.lab=NULL, style="fan",...){
  
	#check if pn.ts
	if(class(psims)!="pn")
		stop("psims must be a pn object (use pn function)")
	if(!is.null(ln))
		if(min(ln)<0 | max(ln)>100)
			stop("all lines must be on a percentiles, indicated by a number between 0 and 100")

	#extract percentiles in psims and identify type
	p<-colnames(psims)
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
  
	p<-as.numeric(p)
	n<-length(p)
	psims<-as.ts(psims)
	#plot polygons
	for(i in 1:floor(n/2)){
    fan.fill(ts1=psims[,i],ts2=psims[,n-i+1],fan.col=fan.col[floor(n/2)-i+1])
	}
	#default lines on deciles
  if(type=="percentile"){
    ln<-ln[ln %in% p]
  }
	if(type=="interval"){
	  if(sum(match(ln, seq(10,90,10))==1:9)==9)
	    ln <- c(50,80,95)
	  ln <- ln[ln %in% p.int]
	  if(!is.null(ln)){
	    ln <- c(ln + (100-ln)/2, 100 - ln - (100-ln)/2)
	    ln<-sort(ln)
	  }
  }
	
	for(i in match(ln,p)){
		lines(psims[,i], col=ln.col, ...)
	}
	if(is.na(sum(match(ln,p))))
	  print("some lines not drawn as not in calculated in psims")
  
  #text by lines
  if(!is.null(txt)){
    txt<-ln[ln %in% p]
    if(type=="interval")  txt<-2*abs(50-txt)
  }	
	if(!is.null(tsp(psims)))	class(psims)<-"pn"
	fan.txt(psims,pn.r=txt,hl.lab=hl.lab)
	box()
}
