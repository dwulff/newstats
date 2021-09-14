
colTrans = function(col,perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue=255, alpha = perc*255)
  }

Cyan = "#235A97" 
Pink = "#EA4B68" 
Gray = "#606060" 
Green = "#6ABA9A"
yellow = "#EACC48"

m1 = 80
m2 = 120
s1 = 25
s2 = 25

png('_sessions/NewStats/image/power.png',width=900, height=1100)

par(mar=c(1,0,30,0))

plot.new();plot.window(xlim=c(0,200),ylim=c(-.0013,.016))

mtext('x',side=1,line=-.2,cex=4)
axis(1,labels=FALSE,line=-3,lwd=8)

mtext(expression(italic('Small N')), side=3, cex=6, col = 'black', line=25, font=1)

xs = seq(0,200,.01)

lines(xs, dnorm(xs, m2, s2), lwd = 12, col = Pink)

xlim = qnorm(.95, m1, s1)
xs = seq(0, xlim, .01)
ys = dnorm(xs, m2, s2)
polygon(c(xs, rev(xs)),c(ys, rep(-.00015, length(ys))), border=NA, col=Pink)

xs = seq(0,200,.01)
lines(xs, dnorm(xs, m1, s1), lwd = 12, col = Gray)


xlim = qnorm(.95, m1, s1)
xs = seq(xlim,200,.01)
ys = dnorm(xs, m1, s1)
polygon(c(xs, rev(xs)),c(ys, rep(-.00015, length(ys))), border=NA, col=Gray)

text(100, .005, label = expression(beta), cex=8, col = 'white')
text(129, .0008, label = expression(alpha), cex=8, col = 'white')
text(61, .00523, label = expression(1-alpha), cex=4, col = Gray)
text(139, .005, label = expression(1-beta), cex=4, col = Pink)

dev.off()



png('_sessions/NewStats/image/effectsize.png',width=600, height=500)

par(mar=c(3,0,0,0))

plot.new();plot.window(xlim=c(0,200),ylim=c(-.00005,.0198))

mtext('x',side=1,line=2,cex=4)
axis(1,labels=FALSE,lwd=8,line=-.5)

xs = seq(0,200,.01)
lines(c(80,80),c(0,max(dnorm(xs, m1, s1))),col=Gray,lwd=6,lend=1)
lines(c(120,120),c(0,max(dnorm(xs, m1, s1))),col=Pink,lwd=6,lend=1)
lines(c(120,150),rep(max(dnorm(xs, m1, s1))/2,2),col=Pink,lwd=6,lend=1)
text(132,.009,labels=expression(sigma),cex=6)

lines(xs, dnorm(xs, m2, s2), lwd = 12, col = Pink)


xs = seq(0,200,.01)
lines(xs, dnorm(xs, m1, s1), lwd = 12, col = Gray)

lines(c(80,120),c(.018,.018),lwd=8,lend=1)
text(100,.0195,labels=expression(Delta),cex=6)

dev.off()












m1 = 80
m2 = 120
s1 = 12
s2 = 12

png('_sessions/NewStats/image/power_large.png',width=900, height=1100)

par(mar=c(1,0,6,0))

plot.new();plot.window(xlim=c(0,200),ylim=c(-.0013,.035))

mtext('x',side=1,line=-.2,cex=4)
axis(1,labels=FALSE,line=-3,lwd=8)

mtext(expression(italic('Large N')), side=3, cex=6, col = 'black', line=0, font=1)

xs = seq(0,200,.01)

lines(xs, dnorm(xs, m2, s2), lwd = 12, col = Pink)

xlim = qnorm(.95, m1, s1)
xs = seq(0, xlim, .01)
ys = dnorm(xs, m2, s2)
polygon(c(xs, rev(xs)),c(ys, rep(-.0003, length(ys))), border=NA, col=Pink)

xs = seq(0,200,.01)
lines(xs, dnorm(xs, m1, s1), lwd = 12, col = Gray)


xlim = qnorm(.95, m1, s1)
xs = seq(xlim,200,.01)
ys = dnorm(xs, m1, s1)
polygon(c(xs, rev(xs)),c(ys, rep(-.0003, length(ys))), border=NA, col=Gray)

text(94.6, .0009, label = expression(beta), cex=6, col = 'white')
text(105.6, .0009, label = expression(alpha), cex=6, col = 'white')
text(75, .00523, label = expression(1-alpha), cex=4, col = Gray)
text(125, .005, label = expression(1-beta), cex=4, col = Pink)

dev.off()

# memnet:::circle(.1,c(.5,.5))
# 
# memnet:::circle_raw(100,deg = 360, rad=.1, orig=c(.5, .5))
# 
# pwr::pwr.t.test(sig.level = .05, power = .95, d = 1)
# 
# png('_sessions/NewStats/image/blurr.png',width=1000,height=1200, bg='transparent')
# plot.new();plot.window(c(0,1),c(0,1))
# rs = seq(.2,2,.00015)
# alphs = seq(.1,1,length.out = length(rs))
# for(i in 1:length(rs)) {
#   lines(memnet:::circle_raw(1000,deg = 360, rad=rs[i], orig=c(.5, .5)),col=rgb(0,0,0,alpha=alphs[i]))
#   }
# dev.off()

d0 = sapply(1:10000, function(x){

  
d = 0

a = rnorm(100,0,1)
b = rnorm(100,d,1)

t.test(a, b)$p.value
})

hist(d0, xlim = c(0,.2), breaks = seq(0,1,.01))


pwr::pwr.t.test(power = .33, sig.level = .05,d=.3)


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

par(mar=c(4.5,7,5,1),mfrow=c(2,1))
plot.new();plot.window(xlim=c(.5,5.5),c(0,.5))
rect((1:5)-.45,0,(1:5)+.45,true_alpha,border=NA, col=Pink)

mtext('Optional stopping',side=3,line=.5,cex=3,font=2.2)
mtext(lens,side=1,at=1:5,cex=2.2)
mtext(seq(0,.5,.05),side=2,at=seq(0,.5,.05),las=1,cex=2.2)
mtext(c('Maximum sample size','True alpha'), side=c(1,2), line=c(2.5,4.3),cex=2.5)
lines(c(.5,5.5),c(.05,.05),col=Gray,lty=2,lwd=5)

plot.new();plot.window(xlim=c(.5,5.5),c(0,.5))
lines(phack_v / sum(phack_v), col = Pink, lwd = 8)
lines(nohack_v / sum(nohack_v), col = Green, lwd = 8)
lines(c(1,5),c(.2,.2), col = Gray, lwd = 5, lty=2)

mtext('P curve',side=3,line=.5,cex=3,font=2.2)
mtext(seq(.01,.05,.01),side=1,at=1:5,cex=2.2)
mtext(seq(0,50,5),side=2,at=seq(0,.5,.05),las=1,cex=2.2)
mtext(c('p-values','Percentage of p-values'), side=c(1,2), line=c(2.5,4.3),cex=2.5)

legend('top',bty='n',lwd=3,legend=c('not hacked','p-hack','no effect'),col=c(Green,Pink,Gray),cex=1.5)

dev.off()


sim_conf = function(d = .2, sd = 50, n = 100){
  
  val = rnorm(n, d * sd, sd)
  m = mean(val)  
  se = sd(val) / sqrt(n)
  conf = m + qt(.975, n-1) * se * c(-1, 1)
  p = t.test(val,alternative = 'two.sided')$p.val
  conf =  t.test(val,alternative = 'two.sided')$conf
  list(conf, p)
  }

res = lapply(1:40,function(x) sim_conf())

confs = sapply(res, `[[`, 1)
ps = sapply(res, `[[`, 2)


png('_sessions/NewStats/image/confint.png',width=700,height=1100)

par(mar=c(2,1,1,1))
plot.new();plot.window(ylim=c(.5,length(ps)+8),xlim=c(-10,40))
for(i in seq(-10,30,10)) lines(c(i,i),c(-2,length(ps)+3),lwd=.5,col='grey85')

xs = seq(-10,30,1)
ys = dnorm(xs,10,5)*70
#lines(xs,ys-8,col=Green,lwd=2)
polygon(c(xs,rev(xs)),c(ys+length(ps)+4,rep(length(ps)+4,length(ys))),col=colTrans(Green,.7), border=NA)

rect(-5,length(ps)+1,15,length(ps)+3,border=NA,col='white')

mtext(seq(-10,30,10),at=seq(-10,30,10),side=1,line=.2,cex=1.5)
text(c(0,10),rep(length(ps)+2,2),c(expression(mu[0]),expression(mu[1])),cex=3)

for(i in 1:length(ps)){
  points(mean(confs[,i]),i, col = ifelse(ps[i]<.05,Pink,Gray),pch=15,cex=1.5)
  lines(confs[,i],c(i,i), col = ifelse(ps[i]<.05,Pink,Gray),lwd=5)
  }

labs = as.character(round(ps,3))
labs[labs == "0"] = '0.0'
labs = stringr::str_sub(labs, 2, nchar(labs))
for(i in 1:length(labs)) if(nchar(labs[i])!=4) labs[i] = paste0(labs[i],paste0(rep(0,4-nchar(labs[i])),collapse=""))
for(i in 1:length(labs)) {
  if(ps[i]<.05 & ps[i] >=.01) labs[i] = paste0(labs[i],'*')
  if(ps[i]<.01 & ps[i] >=.001) labs[i] = paste0(labs[i],'**')
  if(ps[i]<.001) labs[i] = paste0(labs[i],'***')
  }

text(rep(35,length(ps)),1:length(ps),
     labels=labs,col=ifelse(ps<.05,Pink,Gray),
     adj=0,cex=1.5,font=ifelse(ps<.05,1,2))

dev.off()









