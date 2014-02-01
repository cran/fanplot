fan <-
function(data = NULL, data.type="simulations", style = "fan", type = "percentile",
              probs = if(type=="percentile") seq(0.01, 0.99, 0.01) else c(0.5, 0.8, 0.95), 
              start = 1, frequency = 1, anchor = NULL, 
              fan.col = heat.colors, alpha = if (style == "spaghetti") 0.2 else 1, 
              n.fan = NULL,
              ln = if(length(probs)<10) probs else probs[round(probs,5)==round(seq(0.1, 0.9, 0.1),5)],
              med.ln = if(type=="interval") TRUE else FALSE, 
              ln.col = NULL, med.col= NULL,
              rlab = ln, rpos = 4, roffset = 0.1, rcex = 0.8, rcol = NULL, 
              llab = FALSE, lpos = 2, loffset = roffset, lcex = rcex, lcol = rcol, 
              upplab = "U", lowlab = "L", medlab="M",
              n.spag = 30, 
              space = if(style=="boxplot") 1/frequency else 0.9/frequency, ...){
  if(class(data)=="pn"){
    #plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.mcmc), ylab = "Theta")
    if(is.function(fan.col))
      fancol=heat.colors(floor(dim(data)[2]/2))
    else
      fancol=fan.col
    fan0(psims=data, fan.col = fancol, ln.col = fancol[1], ln = if(max(ln)<1) ln*100 else ln, txt =if(max(rlab)<1) rlab*100 else rlab)
    return(print("Using old fan0 function as data is of pn class."))
  }
  if(!(data.type %in% c("values","simulations")))
    stop("data.type must be set to one of: values, simulations")
  if(!(style %in% c("fan","boxfan","spaghetti","boxplot")))
    stop("style must be set to one of: fan, boxfan, spaghetti or boxplot")
  
  if(style=="fan" | style=="boxfan"){
    if(!(type %in% c("percentile","interval")))
      stop("type must be set to one of: percentile or interval")
    #ensure p is okay
    p<-probs
    if(min(p)<0 | max(p)>100)
      stop("all probs must be between 0 and 1 (or 0 and 100)")
    if(max(p)>1)
      p<-p/100
    #make p symetrical
    if(type=="percentile")
      p<-c(p,1-p)
    if(type=="interval" & data.type=="simulations")
      p <- c(p + (1-p)/2, 1 - p - (1-p)/2)
    p<-round(p,5) #i dont know why, but you need this otherwise not unique
    p<-sort(unique(p))
    
    #work out quantiles
    if(data.type=="simulations")
      pp<-apply(data,2,quantile,probs=p)
    if(data.type=="values"){
      pp<-data
      if(type=="percentile" & length(p)!=nrow(pp))
        stop("probs must correspond to the nrows of data if data.type==values and type is percentile")
      if(type=="interval" & length(probs)!=2*nrow(pp)){
        p<-probs
        p<-c(p + (1-p)/2, 1 - p - (1-p)/2)
        p<-sort(p)
        p <- round(p,5)
        rownames(pp)<-p
      }
    }
    n<-nrow(pp)
    if(type=="interval"){
      rownames(pp)[1:(n/2)  ]<-paste0(lowlab, 200*abs(0.5-p)[1:(n/2)]  ,"%")
      rownames(pp)[(1+n/2):n]<-paste0(upplab, 200*abs(0.5-p)[(1+n/2):n],"%")
    }

    #add ancohor
    if(!is.null(anchor)){
      pp<-cbind(rep(anchor,n),pp)
    }
    
    #add ts characterisitcs
    pp<-ts(t(pp), start=start, frequency=frequency)
    if(!is.null(anchor))
      pp<-ts(data.matrix(pp), start=time(lag(pp))[1], frequency=frequency)
    
    #plot polygons
    if(is.null(n.fan))
      fan.col<-fan.col(floor(n/2))
    if(!is.null(n.fan))
      fan.col<-fan.col(n.fan)
    fan.col<-adjustcolor(fan.col,alpha.f=alpha)
    fan.fill<-function(ts1, ts2, fan.col="grey"){
      xx <- cbind(time(ts1),rev(time(ts2))) 
      yy <- cbind(as.vector(ts1),rev(as.vector(ts2)))
      polygon(xx,yy, col=fan.col, border=fan.col)
    }
    #plot(cpi, type = "l", xlim = c(y0-5, y0+3), ylim = c(-2, 7))
    #plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.mcmc), ylab = "Theta")
    #plot(net, ylim=range(net-ips$net.ci, net+ips$net.ci), type = "n")
    
    if(style=="fan"){
      for(i in 1:floor(n/2)){
        fan.fill(ts1=pp[,i],ts2=pp[,n-i+1],fan.col=fan.col[floor(n/2)+1-i])
      }
    }    
    
    #single time series to use for at=time 
    x<-ts(pp[,1], start=start, frequency=frequency)
    if(style=="boxfan"){
      for(i in 1:nrow(pp)){
        for(j in 1:floor(n/2)){
          rect(xleft=time(x)[i]-0.5*space, ybottom=pp[i,j], xright=time(x)[i]+0.5*space, ytop=pp[i,n-j+1], col=fan.col[floor(n/2)+1-j], border=fan.col[floor(n/2)+1-j])
        }
      }
    }
    
    #ensure ln is okay
    if(!is.null(ln)){
      rlab <- ln #otherwise will evaluate after messed with ln
      if(min(ln)<0 | max(ln)>100)
        stop("all ln must be between 0 and 1 (or 0 and 100)")
      if(max(ln)>1)
        ln<-ln/100
      
      #default lines on available pi
      if(type=="interval"){
        ln <- c(ln + (1-ln)/2, 1 - ln - (1-ln)/2)
        ln<-sort(ln)
      }
      if(is.null(ln.col))
        ln.col<-fan.col[1]
      ln<-round(ln,5)
      if(style=="fan"){
        for(i in  match(ln, p))
          lines(pp[,i], col=ln.col)
      }
      if(style=="boxfan"){
        for(i in 1:nrow(pp)){
          for(j in match(ln, p)){
            lines(x=start+(i-1)/frequency+c(-0.5,0.5)*space, y=rep(pp[i,j],2), col=ln.col)
          }
        }
      }
      if(is.na(sum(match(ln,p))))
        print("some lines not plotted as conflict with precentiles given in probs")
    }
    
    #names will be plotted in text
    if(data.type=="values" & type=="percentile")
      colnames(pp)<-paste0(p*100, "%")
    #default right text on available deciles
    if(!is.null(rlab)){
      if(min(rlab)<0 | max(rlab)>100)
        stop("all ln must be between 0 and 1 (or 0 and 100)")
      if(max(rlab)>1)
        rlab<-rlab/100
      if(type=="interval")
        rlab<-c(rlab + (1-rlab)/2, 1 - rlab - (1-rlab)/2)
      rlab<-sort(rlab)
      rlab<-round(rlab, 5)
      if(style=="fan")
        text(tsp(pp)[2], pp[nrow(pp),match(rlab,p)], names(pp[1,match(rlab,p)]), pos=rpos, offset=roffset, cex=rcex, col=rcol)
      if(style=="boxfan")
        text(tsp(pp)[2]+0.5*space, pp[nrow(pp),match(rlab,p)], names(pp[1,match(rlab,p)]), pos=rpos, offset=roffset, cex=rcex, col=rcol)
      if(is.na(sum(match(rlab,p))))
        print("some right labels not plotted as conflict with precentiles given in probs")
    }
    if(is.numeric(llab[1])){
      if(min(llab)<0 | max(llab)>100)
        stop("all ln must be between 0 and 1 (or 0 and 100)")
      if(max(llab)>1)
        llab<-llab/100
      if(type=="interval")
        llab <- c(llab + (1-llab)/2, 1 - llab - (1-llab)/2)
      llab<-sort(llab)
      llab<-round(llab, 5)
      if(style=="fan")
        text(tsp(pp)[1], pp[1,match(llab,p)], names(pp[1,match(llab,p)]), pos=lpos, offset=loffset, cex=lcex, col=lcol)
      if(style=="boxfan")
        text(tsp(pp)[1]-0.5*space, pp[1,match(llab,p)], names(pp[1,match(llab,p)]), pos=lpos, offset=loffset, cex=lcex, col=lcol)
      if(is.na(sum(match(llab,p))))
        print("some left labels not plotted as conflict with precentiles given in probs")
    }
    if(llab==TRUE){
      llab<-rlab
      if(style=="fan")
        text(tsp(pp)[1], pp[1,match(llab,p)], names(pp[1,match(llab,p)]), pos=lpos, offset=loffset, cex=lcex, col=lcol)
      if(style=="boxfan")
        text(tsp(pp)[1]-0.5*space, pp[1,match(llab,p)], names(pp[1,match(llab,p)]), pos=lpos, offset=loffset, cex=lcex, col=lcol)
      if(is.na(sum(match(llab,p))))
        print("some left labels not plotted as conflict with precentiles given in probs")
    }
  }
  
  #add median line
  if(med.ln==TRUE & data.type=="simulations"){
    pm<-apply(data,2,median)
    if(!is.null(anchor))
      pm<-c(anchor,pm)
    pm<-ts(pm, start=start, frequency=frequency)
    if(is.null(med.col))
      med.col<-ln.col
    if(style=="fan" | style=="spaghetti"){
      lines(pm, col=med.col)
    }
    if(style=="boxfan"){
      for(i in 1:nrow(pp)){
        lines(x=(i-1)/frequency+c(-0.5,0.5)*space, y=rep(pm[i],2), col=med.col)
      }
    }
    if(!is.null(rlab) & style %in% c("fan","spaghetti"))
      text(tsp(pm)[2], pm[length(pm)], medlab, pos=rpos, offset=roffset, cex=rcex, col=rcol)
    if(!is.null(rlab) & style=="boxfan")
      text(tsp(pm)[2]+0.5*space, pm[length(pm)], medlab, pos=rpos, offset=roffset, cex=rcex, col=rcol)
    if(any(llab==TRUE,is.numeric(llab)) & style %in% c("fan","spaghetti"))
      text(tsp(pm)[1], pm[1], medlab, pos=lpos, offset=loffset, cex=lcex, col=lcol)
    if(any(llab==TRUE,is.numeric(llab)) & style=="boxfan")
      text(tsp(pm)[1]-0.5*space, pm[1], medlab, pos=lpos, offset=loffset, cex=lcex, col=lcol)
  }
  
  if(style=="spaghetti"){
    pp<-data
    n<-nrow(pp)
    #add ancohor
    if(!is.null(anchor)){
      pp<-cbind(rep(anchor,n),pp)
    }
    #add ts characterisitcs
    pp<-ts(t(pp), start=start, frequency=frequency)
    if(!is.null(anchor))
      pp<-ts(data.matrix(pp), start=time(lag(pp))[1], frequency=frequency)
    if(is.null(ln.col))
      ln.col<-grey(0.5)
    ln.col<-adjustcolor(ln.col,alpha.f=alpha)
    #plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.mcmc), ylab = "Theta")
    for(i in sample(1:n,n.spag))
      lines(pp[,i], col=ln.col)
  }
  
  if(style=="boxplot"){
    if(data.type=="values")
      stop(print("data must be simulations"))
    pp<-data
    n<-ncol(pp)
    if(!is.null(anchor))
      print("anchor ignored for boxplots plots")
    #single time series to use for at=time 
    p<-ts(pp[1,], start=start, frequency=frequency)
    #plot(NULL, type = "n", xlim = c(1, 10),  ylim = range(pp), ylab = "Theta")
    for(i in 1:n)
      boxplot(pp[,i], add=TRUE, at=time(p)[i], boxwex=space, xaxt = "n", yaxt = "n",...)
  }
  box()
}
