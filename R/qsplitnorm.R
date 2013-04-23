qsplitnorm <-
function(p, mean = 0, sd = 1, skew = 0, sd1 = NULL, sd2 = NULL) {
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
  n <- max(length(p),length(mean),length(sd),length(skew),length(sd1),length(sd2))
  f <- rep(NA, n)
  p.star <- f #to deal with long sd
  p.star[] <- p
  psn <- psplitnorm(p.star, mean = mean, sd1 = sd1, sd2 = sd2)
  c <- sqrt(2/pi)/(sd1 + sd2)
  f[p < psn] <- (mean + sd1 * qnorm(p/(c * sqrt(2 * pi) * sd1)) )[p < psn]
  f[p >= psn] <- (mean + sd2 * qnorm((p + c * sqrt(2 * pi) * sd2 - 1)/(c * sqrt(2 * pi) * sd2)))[p >= psn]
  f
}