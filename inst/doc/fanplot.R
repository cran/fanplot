### R code from vignette source 'fanplot.Rnw'

###################################################
### code chunk number 1: fanplot.Rnw:21-23
###################################################
options(SweaveHooks=list(fig=function() par(mar=c(2,4,2,0.1))), continue=" ")
# bltr


###################################################
### code chunk number 2: fanplot.Rnw:35-37
###################################################
library("tsbugs")
head(svpdx)


###################################################
### code chunk number 3: fanplot.Rnw:40-48
###################################################
getOption("SweaveHooks")[["fig"]]()
#plot
plot(svpdx$pdx, type = "l", xaxt = "n", xlab = "Time", ylab = "Return")
#x-axis
svpdx$rdate <- format(svpdx$date, format = "%b %Y")
mth <- unique(svpdx$rdate)
qtr <- mth[seq(1,length(mth),3)]
axis(1, at = match(qtr, svpdx$rdate), labels = qtr, cex.axis = 0.55)
axis(1, at = match(mth, svpdx$rdate), labels = FALSE, tcl = -0.2)


###################################################
### code chunk number 4: fanplot.Rnw:56-68
###################################################
library("R2OpenBUGS")
# write model file:
my1.bug <- dget(system.file("model", "my1.txt", package = "fanplot"))
write.model(my1.bug, "my1.txt")
# take a look:
file.show("my1.txt")
# run openbugs
my1<-bugs(data=list(n=length(svpdx$pdx),y=svpdx$pdx),
          inits=list(list(phistar=0.975,mu=0,itau2=50)),
          param=c("mu","phi","tau","theta"),
          model="my1.txt",
          n.iter = 11000, n.burnin = 1000, n.chains = 1)


###################################################
### code chunk number 5: fanplot.Rnw:73-74
###################################################
th.mcmc <- my1$sims.list$theta


###################################################
### code chunk number 6: fanplot.Rnw:77-79
###################################################
library("fanplot")
th.pn <- pn(sims = th.mcmc)


###################################################
### code chunk number 7: fanplot.Rnw:82-83
###################################################
head(th.pn[,c(1:3, 97:99)])


###################################################
### code chunk number 8: fanplot.Rnw:90-94
###################################################
getOption("SweaveHooks")[["fig"]]()
#empty plot
plot(NULL, type = "n", xlim = c(1, 945), ylim = range(th.pn), ylab = "Theta")
#add fan
fan(th.pn) 


###################################################
### code chunk number 9: fanplot.Rnw:98-108
###################################################
getOption("SweaveHooks")[["fig"]]()
#empty plot with x-axis added later
plot(NULL, type = "l", xlim = c(1, 945), xlab = "Time", xaxt = "n", 
     ylim = range(th.pn), ylab = "Theta")
axis(1, at = match(qtr, svpdx$rdate), labels = qtr, cex.axis = 0.55)
axis(1, at = match(mth, svpdx$rdate), labels = FALSE, tcl = -0.2)
#add fan
fan(th.pn, txt = NA, ln = c(1,10,30,50,70,90,99))
#add text labels for percentiles
fan.txt(th.pn, pn.r = c(1,10,50,90,99))
fan.txt(th.pn, pn.l = c(1,30,70,99))


###################################################
### code chunk number 10: fanplot.Rnw:112-113
###################################################
pal <- colorRampPalette(c("royalblue", "grey", "white"))


###################################################
### code chunk number 11: fanplot.Rnw:116-117
###################################################
fancol <- pal(50)


###################################################
### code chunk number 12: fanplot.Rnw:120-121
###################################################
sigma.pn <- pn(sims = sqrt(exp(th.mcmc)))


###################################################
### code chunk number 13: fanplot.Rnw:124-131
###################################################
getOption("SweaveHooks")[["fig"]]()
#empty plot with x-axis added later
plot(NULL, type = "l", xlim = c(1, 945), xlab = "Time", xaxt = "n", 
     ylim = range(sigma.pn), ylab = "Standard Deviation")
axis(1, at = match(qtr, svpdx$rdate), labels = qtr, cex.axis = 0.55)
axis(1, at = match(mth, svpdx$rdate), labels = FALSE, tcl = -0.2)
#add fan
fan(sigma.pn, fan.col = fancol, ln = c(1, 10, 50, 90, 99))


###################################################
### code chunk number 14: fanplot.Rnw:138-139
###################################################
r <- ts(ew[2:167]/ew[1:166]-1, start=1841)


###################################################
### code chunk number 15: fanplot.Rnw:142-143
###################################################
y <- diff(r)


###################################################
### code chunk number 16: fanplot.Rnw:146-149
###################################################
pop.bug <- sv.bugs(y, k=25, sim=TRUE,
                   sv.mean.prior2 = "dgamma(0.000001,0.000001)", 
                   sv.ar.prior2 = "dunif(-0.999, 0.999)")


###################################################
### code chunk number 17: fanplot.Rnw:152-163
###################################################
library("R2OpenBUGS")
# write model file:
writeLines(pop.bug$bug, "pop.txt")
# take a look:
file.show("pop.txt")
# run openbugs
pop <- bugs(data = pop.bug$data,
            inits = list(list(psi0.star=exp(12), psi1=0.5, itau2=0.5)),
            param = c("psi0", "psi1", "tau", "y.new", "y.sim"),
            model = "pop.txt", 
            n.iter = 11000, n.burnin = 1000, n.chains = 1)


###################################################
### code chunk number 18: fanplot.Rnw:168-169
###################################################
y.mcmc <- pop$sims.list$y.sim


###################################################
### code chunk number 19: fanplot.Rnw:172-174
###################################################
y0 <- tsp(y)[1]
y.pn <- pn(sims = y.mcmc, start = y0)


###################################################
### code chunk number 20: fanplot.Rnw:177-178
###################################################
str(y.pn)


###################################################
### code chunk number 21: fanplot.Rnw:181-188
###################################################
getOption("SweaveHooks")[["fig"]]()
#empty plot
plot(NULL, type = "l", xlim = range(time(y.pn)), xlab = "Time", 
     ylim = range(y), ylab = "Expected Model Fit")
#add fan
fan(y.pn, ln = c(1, 10, 90, 99))
#add data
lines(diff(r), lwd = 2)


###################################################
### code chunk number 22: fanplot.Rnw:192-193
###################################################
y.pn2 <- pn(sims = y.mcmc, p = c(1, 20, 40, 60, 80, 99), start = y0)


###################################################
### code chunk number 23: fanplot.Rnw:198-205
###################################################
getOption("SweaveHooks")[["fig"]]()
#empty plot
plot(NULL, type = "l", xlim = range(time(y.pn)), xlab = "Time", 
     ylim = range(diff(r)), ylab = "Expected Model Fit")
#add fan
fan(y.pn2) 
#add data
lines(diff(r), lwd = 2)


###################################################
### code chunk number 24: fanplot.Rnw:210-216
###################################################
ynew.mcmc <- pop$sims.list$y.new
rnew.mcmc <- apply(ynew.mcmc, 1, diffinv, xi = tail(r,1))
rnew.mcmc <- t(rnew.mcmc[-1,])

pnew.mcmc <- apply(1+rnew.mcmc, 1, cumprod) * tail(ew,1)
pnew.mcmc <- t(pnew.mcmc)


###################################################
### code chunk number 25: fanplot.Rnw:219-221
###################################################
r0 <- tsp(r)[2]
rnew.pn <- pn(sims = rnew.mcmc, start = r0 + 1)


###################################################
### code chunk number 26: fanplot.Rnw:224-234
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfrow = c(1 ,2))
#sample of underlying simulation data
plot(r, ylim = range(r), xlim = c(1940, 2040), lwd = 2, 
     ylab = "Population Growth Rate")
for (i in 1:30) lines(ts(rnew.mcmc[i, ], r0 + 1), col = "grey")
#plot r
plot(r, ylim = range(r), xlim = c(1940, 2040), lwd = 2, 
     ylab = "Population Growth Rate")
#add fan
fan(rnew.pn) 


###################################################
### code chunk number 27: fanplot.Rnw:238-242
###################################################
p0 <- tsp(ew)[2]
pnew.pn <- pn(sims = pnew.mcmc/1e+06, start = p0 + 1)
pnew.pn2 <- pn(sims = pnew.mcmc/1e+06, p = c(1, 10, 40, 50), 
               anchor = tail(ew,1)/1e+06, start = p0)


###################################################
### code chunk number 28: fanplot.Rnw:247-258
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mfrow = c(1 ,2))
#plot ew
plot(ew/1e+06, ylim = c(40, 80), xlim = c(1940, 2040), lwd = 2, 
     ylab = "Population (m)")
#add fan
fan(pnew.pn)
#plot ew
plot(ew/1e+06, ylim = c(40, 80), xlim = c(1940, 2040), lwd = 2, 
     ylab = "Population (m)")
#add fan
fan(pnew.pn2, ln.col = "black") 


