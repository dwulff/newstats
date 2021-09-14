

exp1 = readxl::read_excel('~/Downloads/exp1.xlsx')
names(exp1)

exp1 = exp1 %>% select(-Session, -Date,-Session.Length) %>%
  mutate(StartTime = hour(as_datetime(StartTime,format="%I:%M%p"))) %>%
  rename(Uhrzeit = StartTime,
         Geschlecht = Participant.Sex,
         Alter = Participant.Age,
         Geschlecht_präsentiert  = ExpSex,
         Stimulus_seeking = Stimulus.Seeking,
         erotic = Erotic.Hits.PC,
         control = Control.Hits.PC) %>%
  gather(Bedingung, Trefferrate, c(erotic, control)) %>%
  mutate(Anzahl_trials = case_when(
    condition == 'erotic' ~ 12,
    TRUE ~ 24
  )) %>%
  select(Geschlecht, Alter, Stimulus_seeking, Uhrzeit,Bedingung,Alter,Anzahl_trials,Trefferrate)


# exp2 = readxl::read_excel('~/Downloads/exp2.xlsx')
# 
# exp2 = exp2 %>% select(-Session, -Date,-Session.Length) %>%
#   mutate(StartTime = hour(as_datetime(StartTime,format="%I:%M%p"))) %>%
#   rename(hour_of_day = StartTime,
#          gender = Participant.Sex,
#          age = Participant.Age,
#          depicted_gender  = ExpSex,
#          stimulus_seeking = Stimulus.Seeking,
#          hit_rate = Hits.PC,
#          n_trials = 36) %>%
#   select(gender, age, stimulus_seeking, hour_of_day,n_trials,hit_rate)

exp2 = exp1
exp2$hit_rate = sample(exp2$hit_rate)

#t.test(exp2$hit_rate,mu = 50)


write_csv(exp1,'_sessions/NewStats/1_Data/psi_exp1_en.csv')
write_csv(exp2,'_sessions/NewStats/1_Data/psi_exp2_en.csv')

require(tidyverse)
exp1 = read_csv('_sessions/NewStats/1_Data/psi_exp1_en.csv')
exp2 = read_csv('_sessions/NewStats/1_Data/psi_exp2_en.csv')

exp1 = exp1 %>% rename(
  Geschlecht= gender,
  Alter= age,
  Stimulus_seeking= stimulus_seeking,
  Uhrzeit= hour_of_day,
  Bedingung= condition,
  Präsentiertes_geschlecht= depicted_gender,
  Anzahl_trials= n_trials,
  Trefferrate= hit_rate
  )

exp2 = exp2 %>% rename(
  Geschlecht= gender,
  Alter= age,
  Stimulus_seeking= stimulus_seeking,
  Uhrzeit= hour_of_day,
  Bedingung= condition,
  Präsentiertes_geschlecht= depicted_gender,
  Anzahl_trials= n_trials,
  Trefferrate= hit_rate
)

write_csv(exp1,'_sessions/NewStats/1_Data/psi_exp1.csv')
write_csv(exp2,'_sessions/NewStats/1_Data/psi_exp2.csv')



Geschlecht, Alter, Stimulus_seeking, Uhrzeit,Bedingung,Alter,Anzahl_trials,Trefferrate

cat(paste0('= ',names(exp1),','),sep='\n')


dim(exp1)
dim(exp2)


Cyan = "#235A97" 
Pink = "#EA4B68" 
Gray = "#606060" 
Green = "#6ABA9A"
yellow = "#EACC48"

pinkgray = memnet::cmix(Pink, Gray, .5)

cols = c(Pink, pinkgray, Gray)

png('_sessions/NewStats/image/tdistr.png',width=1800,height=440)

a = pwr::pwr.t.test(d = .5, power=.8)
pwr::plot.power.htest(a)

par(mar=c(3.9,0,0,0))
plot.new();plot.window(c(-4.2,4.2),c(0,.4))

x = seq(-4.2,4.2,.01)
lines(x,dt(x, df = 100),lwd=20,col=cols[1])
lines(x,dt(x, df = 5),lwd=20,col=cols[2])
lines(x,dt(x, df = 2),lwd=20,col=cols[3])

pos = qt(c(.005,.025,.15,.5,.85,.975,.995),5)
axis(1,lwd=6,labels = F,lend=2,at = pos)
mtext(c(
  expression(t[paste(".5%, df=",5)]),
  expression(t[paste("2.5%, df=",5)]),
  expression(t[paste("15%, df=",5)]),
  expression(t[paste("50%, df=" ,5)]),
  expression(t[paste("85%, df=" ,5)]),
  expression(t[paste("97.5%, df=",5)]),
  expression(t[paste("99.5%, df=",5)])), at = pos,line=3,side=1,adj=.5,cex=3.2)

xpos = c(2.7,3.1)
ypos = c(.3,.25,.2)
lines(xpos,rep(ypos[1],2),lend=2,col=cols[1],lwd=20)
lines(xpos,rep(ypos[2],2),lend=2,col=cols[2],lwd=20)
lines(xpos,rep(ypos[3],2),lend=2,col=cols[3],lwd=20)
text(rep(xpos[2],3)+.15,ypos,
     labels = c(expression(paste(italic(df)," = 100")),
                expression(paste(italic(df)," = 5")),
                expression(paste(italic(df)," = 2"))),adj=0,cex=3)

dev.off()

