fan0 <-
function(psims, fan.col=heat.colors(floor(dim(psims)[2]/2)), ln.col=fan.col[1],
	ln=seq(10,90,10), txt=ln, ...){
  
	#check if pn.ts
	if(class(psims)!="pn")
		stop("psims must be a pn object (use pn function)")
	if(!is.null(ln))
		if(min(ln)<0 | max(ln)>100)
			stop("all lines must be on a percentiles, indicated by a number between 0 and 100")

	#extract percentiles in psims and identify type
	p<-colnames(psims)
	p<-gsub("%","",p)
  p<-as.numeric(p)
	n<-length(p)
	psims<-as.ts(psims)
	#plot polygons
	for(i in 1:floor(n/2)){
    fan.fill(ts1=psims[,i],ts2=psims[,n-i+1],fan.col=fan.col[floor(n/2)-i+1])
	}
	#default lines on deciles
  ln<-ln[ln %in% p]
	for(i in match(ln,p)){
		lines(psims[,i], col=ln.col, ...)
	}
	if(is.na(sum(match(ln,p))))
	  print("some lines not drawn as not in calculated in psims")
  
  #text by lines
  if(!is.null(txt)){
    txt<-ln[ln %in% p]
  }	
	if(!is.null(tsp(psims)))	class(psims)<-"pn"
	fan.txt(psims,pn.r=txt)
	box()
}
