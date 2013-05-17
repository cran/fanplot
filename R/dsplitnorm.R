dsplitnorm <-
function(x, mean = 0, sd = 1, skew = 0, sd1 = NULL, sd2 = NULL) {
  var0 <- sd^2
  if (!is.null(sd1)) 
    var1 <- sd1^2
  if (!is.null(sd2)) 
    var2 <- sd2^2
  if (sum(is.null(sd1), is.null(sd2)) == 1) 
    stop("give either sd of both sides (sd1 and sd2) or neither (sd only)")
  if (is.null(sd1) & is.null(sd2)) {
    var1 <- var0/(1 + skew)
    var2 <- var0/(1 - skew)
    sd1 <- sqrt(var1)
    sd2 <- sqrt(var2)
  }
  if (skew > 1 & skew < 1) 
    stop("skew must be between -1 and 1")
  n <- max(length(x),length(mean),length(sd),length(skew),length(sd1),length(sd2))
  f <- rep(NA, n)
  c <- sqrt(2/pi)/(sd1 + sd2)
  f[x <= mean] <- c * exp((-(x[x <= mean] - mean)^2)/(2 * var1))
  f[x >  mean] <- c * exp((-(x[x >  mean] - mean)^2)/(2 * var2))
  return(f)
}
