

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
  b = rnorm(nmax,0,1)
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
true_alpha_mean = sapply(lens, function(x) {print(x);sapply(rep(x,100), phack, d = 0)})

colMeans(true_alpha_mean)
exp(colMeans(log(true_alpha_mean)))


png('_sessions/sequential_analysis/image/P-hacking_frequentist.png',width=800,height=1200)

par(mar=c(4.5,7,5,1), mfrow=c(2, 1))
plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,true_alpha,border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','False positive rate'), side=c(1,2), line=c(2.5,4.3),cex=2.5)
lines(c(.5,5.5),c(.05,.05),col=Gray,lty=2,lwd=5)

plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,exp(colMeans(log(true_alpha_mean))),border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','Average p-value'), side=c(1,2), line=c(2.5,4.3),cex=2.5)
lines(c(.5,5.5),c(.05,.05),col=Gray,lty=2,lwd=5)

dev.off()






test@bayesFactor$bf

bayes_phack = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,d,1)
  for(i in 2:nmax){
    test = ttestBF(a[1:i], b[1:i])
    bf = exp(test@bayesFactor$bf)
    if(bf > 3) break
  }
  bf
}

bayes_phack_both = function(d = 0, nmax = 100){
  a = rnorm(nmax,0,1)
  b = rnorm(nmax,0,1)
  for(i in 2:nmax){
    test = ttestBF(a[1:i], b[1:i])
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

# true_bf = sapply(lens, function(x) {print(x);sapply(rep(x,50), bayes_phack, d = 0)})
# saveRDS(true_bf, '_sessions/sequential_analysis/image/bf_sim.RDS')

# true_bf = sapply(lens, function(x) {print(x);sapply(rep(x,50), bayes_phack_both, d = 0)})
# saveRDS(true_bf, '_sessions/sequential_analysis/image/bf_both_sim.RDS')

true_bf = readRDS('_sessions/sequential_analysis/image/bf_sim.RDS')
true_bf_both = readRDS('_sessions/sequential_analysis/image/bf_both_sim.RDS')

true_bf_nohack = sapply(lens, function(x) {print(x);sapply(rep(x,100), no_bayes_phack, d = 0)})

exp(colMeans(log(true_bf)))
exp(colMeans(log(true_bf_both)))
exp(colMeans(log(true_bf_nohack)))

colMeans(true_bf>3)
colMeans(true_bf>1)


png('_sessions/sequential_analysis/image/P-hacking_bayes.png',width=800,height=1200)

par(mar=c(4.5,7,5,1), mfrow=c(2, 1))
plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,colMeans(true_bf>3),border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','False positive rate'), side=c(1,2), line=c(2.5,4.3),cex=2.5)
lines(c(.5,5.5),c(.05,.05),col=Gray,lty=2,lwd=5)

plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,exp(colMeans(log(true_bf))),border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','Average BF10'), side=c(1,2), line=c(2.5,4.3),cex=2.5)

dev.off()


true_bf_nohack = sapply(lens, function(x) {print(x);sapply(rep(x,100), no_bayes_phack, d = 0)})









simulate_sequential_onesample <- function(stops = 2:100,
                                          alpha = .05,
                                          mus = c(0,0), 
                                          sigma = 1, 
                                          repeats = 1000){
  stops = c(0, stops)
  ps <- numeric(repeats)
  ns <- numeric(repeats)
  for(i in 1:repeats){
    x = rep(NA, max(stops))
    y = rep(NA, max(stops))
    for(j in 1:(length(stops)-1)){
      pos = (stops[j]+1):stops[j+1]
      x[pos] <- rnorm(length(pos), mus[1], sigma)
      y[pos] <- rnorm(length(pos), mus[2], sigma)
      p <- t.test(x, y)$p.value
      if(p < alpha) break
    }
    ps[i] <- p
  }
  mean(ps<alpha)
}


  simulate_sequential_onesample(alpha = .0145, stops = 2:10, repeats = 5000)
  simulate_sequential_onesample(alpha = .0098, stops = 2:20, repeats = 5000)
  simulate_sequential_onesample(alpha = .0055, stops = 2:50, repeats = 5000)
  simulate_sequential_onesample(alpha = .0050, stops = 2:70, repeats = 5000)
  simulate_sequential_onesample(alpha = .0047, stops = 2:100, repeats = 5000)

alphas = c(.0145, .0098, .0055, .0050, .0047)

powers = c(
  simulate_sequential_onesample(alpha = .014, stops = 2:10, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .0098, stops = 2:20, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .0055, stops = 2:50, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .0050, stops = 2:70, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .0047, stops = 2:100, repeats = 5000, mus = c(0,.3))
  )


powers_normal = c(
  simulate_sequential_onesample(alpha = .05, stops = 10, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .05, stops = 20, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .05, stops = 50, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .05, stops = 70, repeats = 5000, mus = c(0,.3)),
  simulate_sequential_onesample(alpha = .05, stops = 100, repeats = 5000, mus = c(0,.3))
)


png('_sessions/sequential_analysis/image/const_alpha.png',width=800,height=1200)

par(mar=c(4.5,7,5,1), mfrow=c(2, 1))
plot.new();plot.window(xlim=c(.5,5.5),c(0,.05))
rect((1:5)-.45,0,(1:5)+.45,alphas,border=NA, col=Pink)

mtext(c(10, 20, 50, 70, 100),side=1,at=1:5,cex=2.2)
mtext(seq(0,.05,.005),side=2,at=seq(0,.05,.005),las=1,cex=2.2)
mtext(c('Maximum sample size',expression(alpha)), side=c(1,2), line=c(2.5,5),cex=2.5)

plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,powers_normal,border=NA, col=Gray)
rect((1:5)-.45,0,(1:5)+.45,powers,border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','Power'), side=c(1,2), line=c(2.5,5),cex=2.5)

dev.off()





spend = function(a, t) 4 - 4*pnorm(qnorm(1-a/4)/sqrt(t))

spend(.05, seq(0,1,.1)) %>% plot()


# incremental one sample t-test simulation 
simulate_sequential_prop <- function(stops,
                                       alphas,
                                       mus = c(0,0), 
                                       sigma = 1, 
                                       repeats = 1000){
  if(length(stops) != length(alphas)) stop("stops and alphas need to be equally long.")
  stops = c(0, stops)
  ps <- numeric(repeats)
  ns <- numeric(repeats)
  for(i in 1:repeats){
    x = rep(NA, max(stops))
    y = rep(NA, max(stops))
    for(j in 1:(length(stops)-1)){
      pos = (stops[j]+1):stops[j+1]
      x[pos] <- rnorm(length(pos), mus[1], sigma)
      y[pos] <- rnorm(length(pos), mus[2], sigma)
      p <- t.test(x, y)$p.value
      if(p < alphas[j]) break
    }
    ps[i] <- p < alphas[j] 
    ns[i] <- stops[j+1]
  }
  # out
  cbind("signficant" = ps, "sample_size" = ns)
}


s1 = simulate_sequential_prop(alpha = spend(.05, (2:10)/10), stops = 2:10, repeats = 5000, mus = c(0,0))
s2 = simulate_sequential_prop(alpha = spend(.05, (2:20)/20), stops = 2:20, repeats = 5000, mus = c(0,0))
s3 = simulate_sequential_prop(alpha = spend(.05, (2:50)/50), stops = 2:50, repeats = 5000, mus = c(0,0))
s4 = simulate_sequential_prop(alpha = spend(.05, (2:70)/70), stops = 2:70, repeats = 5000, mus = c(0,0))
s5 = simulate_sequential_prop(alpha = spend(.05, (2:100)/100), stops = 2:100, repeats = 5000, mus = c(0,0))
  

e1 =  simulate_sequential_prop(alpha = spend(.05, (2:10)/10), stops = 2:10, repeats = 5000, mus = c(0,.3))
e2 =  simulate_sequential_prop(alpha = spend(.05, (2:20)/20), stops = 2:20, repeats = 5000, mus = c(0,.3))
e3 =  simulate_sequential_prop(alpha = spend(.05, (2:50)/50), stops = 2:50, repeats = 5000, mus = c(0,.3))
e4 =  simulate_sequential_prop(alpha = spend(.05, (2:70)/70), stops = 2:70, repeats = 5000, mus = c(0,.3))
e5 =  simulate_sequential_prop(alpha = spend(.05, (2:100)/100), stops = 2:100, repeats = 5000, mus = c(0,.3))


n1 = simulate_sequential_prop(alpha = mean(s1[,1]), stops = 10, repeats = 5000, mus = c(0,.3))
n2 = simulate_sequential_prop(alpha = mean(s2[,1]), stops = 20, repeats = 5000, mus = c(0,.3))
n3 = simulate_sequential_prop(alpha = mean(s3[,1]), stops = 50, repeats = 5000, mus = c(0,.3))
n4 = simulate_sequential_prop(alpha = mean(s4[,1]), stops = 70, repeats = 5000, mus = c(0,.3))
n5 = simulate_sequential_prop(alpha = mean(s5[,1]), stops = 100, repeats = 5000, mus = c(0,.3))

power_normal = c(mean(n1[,1]), mean(n2[,1]), mean(n3[,1]), mean(n4[,1]), mean(n5[,1]))
power_spend = c(mean(e1[,1]), mean(e2[,1]), mean(e3[,1]), mean(e4[,1]), mean(e5[,1]))
ns_spend = c(mean(e1[,2])/10, mean(e2[,2])/20, mean(e3[,2])/50, mean(e4[,2])/70, mean(e5[,2])/100)

png('_sessions/sequential_analysis/image/spend_alpha.png',width=800,height=1200)

par(mar=c(4.5,7,5,1), mfrow=c(2, 1))
plot.new();plot.window(xlim=c(0,1),c(0,.05))

lines(seq(0,1,.01), spend(.05, seq(0,1,.01)), col = Pink, lwd=8)

mtext(seq(0,1,.2),side=1,at=seq(0,1,.2),cex=2.2)
mtext(seq(0,.05,.005),side=2,at=seq(0,.05,.005),las=1,cex=2.2)
mtext(c('Proportion of maximum size',expression(alpha)), side=c(1,2), line=c(2.5,5),cex=2.5)

plot.new();plot.window(xlim=c(.5,5.5),c(0,.7))
rect((1:5)-.45,0,(1:5)+.45,power_normal,border=NA, col=Gray)
rect((1:5)-.45,0,(1:5)+.45,power_spend,border=NA, col=Pink)

mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.7,.1),side=2,at=seq(0,.7,.1),las=1,cex=2.2)
mtext(c('Maximum sample size','Power'), side=c(1,2), line=c(2.5,5),cex=2.5)

text(1:5, rep(.05, 5), labels = paste0(round(ns_spend*100), "%"), col="white", cex=2)

dev.off()