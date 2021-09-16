

Cyan = "#235A97" 
Pink = "#EA4B68" 
Gray = "#606060" 
Green = "#6ABA9A"
yellow = "#EACC48"

nohack = function(d = 0, n = 100){
  a = rnorm(n,0,1)
  b = rnorm(n,d,1)
  t.test(a, b)$p.value
}

phack = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,d,1)
  for(i in 2:nmax){
    p = t.test(a[1:i], b[1:i])$p.value
    if(p < .05) break
  }
  p
}


nohack_res = sapply(rep(100,10000), nohack, d = .215943)
phack_res = sapply(rep(100,10000), phack, d = 0)

nohack_v = hist(nohack_res, breaks = seq(0,1,.01), plot = F)[["counts"]][1:5]
phack_v = hist(phack_res, breaks = seq(0,1,.01), plot = F)[["counts"]][1:5]

lens = c(10,50,100,500,1000)
true_alpha = sapply(lens, function(x) {print(x);mean(sapply(rep(x,100), phack, d = 0)<.05)})


png('_sessions/NewStats/image/P-hacking.png',width=800,height=1200)

par(mar=c(4.5,7,5,1))
plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,true_alpha,border=NA, col=Pink)

mtext('Optional stopping',side=3,line=.5,cex=3,font=2.2)
mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.5,.05),side=2,at=seq(0,.5,.05),las=1,cex=2.2)
mtext(c('Maximum sample size','True alpha'), side=c(1,2), line=c(2.5,4.3),cex=2.5)
lines(c(.5,5.5),c(.05,.05),col=Gray,lty=2,lwd=5)

dev.off()


test@bayesFactor$bf

bayes_phack = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,d,1)
  for(i in 2:nmax){
    test = ttestBF(a[1:i], b[1:i])
    p = t.test(a[1:i], b[1:i])$p.value
    bf = exp(test@bayesFactor$bf)
    if(bf > 3) break
  }
  bf
}

bayes_phack_both = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,d,1)
  for(i in 2:nmax){
    test = ttestBF(a[1:i], b[1:i])
    p = t.test(a[1:i], b[1:i])$p.value
    bf = exp(test@bayesFactor$bf)
    if(bf > 3 | bf < 1/3) break
  }
  bf
}

no_bayes_phack = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,d,1)
  test = ttestBF(a[1:i], b[1:i])
  p = t.test(a, b)$p.value
  bf = exp(test@bayesFactor$bf)
  bf
}

true_bf = sapply(lens, function(x) {print(x);sapply(rep(x,50), bayes_phack, d = 0)})
saveRDS(true_bf, '_sessions/sequential_analysis/image/bf_sim.RDS')

true_bf = sapply(lens, function(x) {print(x);sapply(rep(x,50), bayes_phack_both, d = 0)})
saveRDS(true_bf, '_sessions/sequential_analysis/image/bf_both_sim.RDS')

true_bf = readRDS('_sessions/sequential_analysis/image/bf_sim.RDS')
true_bf_both = readRDS('_sessions/sequential_analysis/image/bf_both_sim.RDS')

exp(colMeans(log(true_bf)))

colMeans(true_bf>1)
colMeans(true_bf>1)


true_bf_nohack = sapply(lens, function(x) {print(x);sapply(rep(x,100), no_bayes_phack, d = 0)})

