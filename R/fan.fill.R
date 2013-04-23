fan.fill <-
function(ts1, ts2, fan.col="grey",...){
	xx <- cbind(time(ts1),rev(time(ts2))) 
	yy <- cbind(as.vector(ts1),rev(as.vector(ts2)))
  polygon(xx,yy, col=fan.col, border=fan.col,...)
}
