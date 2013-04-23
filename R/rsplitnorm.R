rsplitnorm <-
function(n, mean = 0, sd = 1, skew = 0, sd1 = NULL, sd2 = NULL) {
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
  u <- runif(n)
  f <- qsplitnorm(u, mean = mean, sd = sd, skew = skew, sd1 = sd1, sd2 = sd2)
  f
}
