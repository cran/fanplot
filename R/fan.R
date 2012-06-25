fan <-
function(psims, fan.col=heat.colors(floor(dim(psims)[2]/2)), ln.col=fan.col[1],
	ln=NULL, txt=NULL, ...){
	#check if pn.ts
	if(class(psims)!="pn")
		stop("psims must be a pn object (use pn)")
	if(!is.null(ln))
		if(min(ln)<0 | max(ln)>100)
			stop("all lines must be on a percentiles, indicated by a number between 0 and 100")
	if(!is.null(tsp(psims)))	psims<-as.ts(psims)
	#extract percentiles in psims
	p<-colnames(psims)
	p<-gsub("%","",p)
	p<-as.numeric(p)
	if(!is.null(ln))
		if(any(ln %in% p==F))
			stop("all lines must be percentiles existing in psims")
	n<-length(p)
	#plot polygons
	for(i in 1:floor(n/2)){
    fan.fill(ts1=psims[,i],ts2=psims[,n-i+1],fan.col=fan.col[floor(n/2)-i+1])
	}
	#deciles for lines default
	d<-seq(10,90,10)
	ln.def<-c(d[d %in% p]) 
	if(is.null(ln) & all(ln.def %in% p))	ln<-ln.def
	for(i in match(ln,p)){
		lines(psims[,i], col=ln.col, ...)
	}
	#text by lines
	if(is.null(txt))	txt<-ln
	if(!is.null(tsp(psims)))	class(psims)<-"pn"
	if(any(!is.na(txt)))	fan.txt(psims,pn.r=txt)
	box()
}
