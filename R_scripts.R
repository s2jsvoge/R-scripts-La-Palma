# La Palma

# Precipitation ####
#setwd("D:/Global Change Ecology/Summer_Winter Schools/La Palma/Results")
setwd("D:/Global Change Ecology/Summer_Winter Schools/La Palma/Data")

prec<-read.csv("prec_devices.csv",header=T,sep=";") # change separator if needed
control<-read.csv("prec_control.csv",header=T,sep=";",dec=",")
net<-read.csv("prec_nets.csv",header=T,sep=";",dec=",")
str(prec)
str(control)
str(net)

# Durchschnitt/Standardabweichung zu jedem Tag/Standort der Kontrolldevices
mean_a1<-mean(control[37:39,3])
mean_a2<-mean(control[40:42,3])
mean_a3<-mean(control[43:45,3])
mean_a1;mean_a2;mean_a3
sd_a1<-sd(control[37:39,3])
sd_a2<-sd(control[40:42,3])
sd_a3<-sd(control[43:45,3])
sd_a1;sd_a2;sd_a3
mean_b1<-mean(control[47:48,3]) # Hier nur 2 Werte vorhanden
mean_b2<-mean(control[49:51,3])
mean_b3<-mean(control[52:54,3])
mean_b1;mean_b2;mean_b3
sd_b1<-sd(control[47:48,3]) # Hier nur 2 Werte vorhanden
sd_b2<-sd(control[49:51,3])
sd_b3<-sd(control[52:54,3])
sd_b1;sd_b2;sd_b3
#' sd from site 1 is fine, but other sites have large sd
#' in site 3 its always the third one which is large;
#' in site 2 it varies: still, we don't have a better option but
#' we have to consider that our results are afflicted by large uncertainties

# Confidence intervals (they would require normal distribution, we neglect that)
t1<-t.test(control[37:39,3])$conf.int
t2<-t.test(control[40:42,3])$conf.int
t3<-t.test(control[43:45,3])$conf.int # includes zero, i.e. it is not meaningful at all
t4<-t.test(control[47:48,3])$conf.int 
t5<-t.test(control[49:51,3])$conf.int
t6<-t.test(control[52:54,3])$conf.int
t_low<-c(t1[1],t2[1],t3[1],t4[1],t5[1],t6[1])
t_up<-c(t1[2],t2[2],t3[2],t4[2],t5[2],t6[2])
con_mean<-c(mean_a1,mean_a2,mean_a3,mean_b1,mean_b2,mean_b3)
con_sd<-c(sd_a1,sd_a2,sd_a3,sd_b1,sd_b2,sd_b3)
cplot1<-barplot(con_mean,ylim=c(-50,600))
arrows(cplot1,t_low,cplot1,t_up , angle=90, length=.05, code=3)
title("Mean with confidence intervals for all 3 sites and 2 days")
# Dies bedeutet dass unsere Methode VIEL zu unsicher ist
# Der Fehler ist so riesig, dass er s�mtliche potentielle Ergebnisse nichtig macht
# die hier gezeigten Konfidenzintervalle sind wohl sogar noch zu klein, da normalverteilung nicht gegeben ist

# 21.03.16 is not included so far; just ignore it?

# Calculate "fog precipitation" der Devices
a1<-(prec[109:117,4]-mean_a1) # ist dies die beste Methode? (die Kontrolldevices sind arg unterschiedlich)
a2<-(prec[118:126,4]-mean_a2)
a3<-(prec[127:135,4]-mean_a3)
b1<-(prec[136:144,4]-mean_b1)
b2<-(prec[145:153,4]-mean_b2)
b3<-(prec[154:162,4]-mean_b3)
# Calculate "fog precipitation" der Netze
na1<-(net[37:39,3]-mean_a1)
na2<-(net[40:42,3]-mean_a2)
na3<-(net[43:45,3]-mean_a3)
nb1<-(net[46:48,3]-mean_b1)
nb2<-(net[49:51,3]-mean_b2)
nb3<-(net[52:54,3]-mean_b3)

m1_19<-mean(a1);m2_19<-mean(a2);m3_19<-mean(a3)
m123_19<-c(m1_19,m2_19,m3_19)
m1_20<-mean(b1);m2_20<-mean(b2);m3_20<-mean(na.omit(b3))
m123_20<-c(m1_20,m2_20,m3_20) 
mn123_19<-c(mean(na1),mean(na2),mean(na3)) # mean nets
mn123_20<-c(mean(nb1),mean(nb2),mean(nb3))
s1_19<-sd(a1);s2_19<-sd(a2);s3_19<-sd(a3)
s123_19<-c(s1_19,s2_19,s3_19) # sd vary quite a bit
s1_20<-sd(b1);s2_20<-sd(b2);s3_20<-sd(na.omit(b3))
s123_20<-c(s1_20,s2_20,s3_20) #2nd day has much larger sd
#sn123_19<-c(sd(na1),sd(na2),sd(na3)) # sd nets
#sn123_20<-c(sd(nb1),sd(nb2),sd(nb3))

summary(a1);summary(a2);summary(a3);summary(b1);summary(b2);summary(b3)
# Die Verteilung ist doch recht verschieden

# Plot der means der fog precipitation (devices) mit sd
#vllt nur Punkte statt barplots verwenden
boxplot(a1,a2,a3)
title("Day 1 Fog precipitation")
boxplot(b1,b2,b3)
title("Day 2 Fog precipitation")
bplot1<-barplot(m123_19,ylim=c(-200,350))
arrows(bplot1,m123_19 - s123_19,bplot1,m123_19 + s123_19, angle=90, length=.05, code=3) # barplot mit standardabweichung
title("Day 1 Fog precipiation mean with standard deviation")
#bplot1<-barplot(c(m1_19,m2_19,m3_19))
bplot2<-barplot(m123_20,ylim=c(-500,550))
arrows(bplot2,m123_20 - s123_20,bplot1,m123_20 + s123_20, angle=90, length=.05, code=3) # barplot mit standardabweichung
title("Day 2 Fog precipiation mean with standard deviation")
## sign. von 0? siehe Test 2

# Plot der means der fog precipitation (nets) mit sd
plot(na1,col="blue",ylim=c(-400,600),type="l")
title("Plot der means der fog precipitation (nets) mit sd")
#abline(na1(1),na1(3))
lines(na2,col="red")
points((na3),col="green")
lines(nb1,col="cyan")
lines(nb2,col="VioletRed3")
lines(nb3,col="chartreuse")
#' This data is worthless. This is quite problematic. 
#' Even to calculate a slope and use it for correlation purposes is highly debatable.
#' Also it is just way too less data.

# Prerequisite Test f�r Normalverteilung
shapiro.test(a1);shapiro.test(a2);shapiro.test(a3) # a2 ist wohl nicht normalverteilt
shapiro.test(b1);shapiro.test(b2);shapiro.test(b3) # b1 und b2 sind wohl nicht normalverteilt
shapiro.test(control[37:39,3]) # not normally distributed
shapiro.test(control[40:42,3]) # fine
shapiro.test(control[43:45,3]) # not normally distributed
shapiro.test(control[47:48,3]); # not applicable
shapiro.test(control[49:51,3]); # fine (but not really if you look at plot)
shapiro.test(control[52:54,3]) # fine
# Entscheidung f�r Wilcoxon-Test

# Test 1: Unterscheiden sich Control und Forest plots sign. voneinander?
wilcox.test(a1,control[37:39,3]) # Yes
wilcox.test(a2,control[40:42,3]) # Yes
wilcox.test(a3,control[43:45,3]) # Yes
wilcox.test(b1,control[47:48,3]) # No, but with only two b1-values this has little meaning
wilcox.test(b2,control[49:51,3]) # No, strange, actually they differ a lot
wilcox.test(b3,control[52:54,3]) # Yes
# No real conclusion can be drawn, Control plots are just too little to be representative

# Data from site1 could be tested for the third day to state 
control[55:57]

# Test 2: Ist die vermutete Fog precipitation von 0 verschieden? 
# D.h. gibt es �berhaupt einen Effekt der nicht rein zuf�llig bedingt ist?
wilcox.test(a1);wilcox.test(a2);wilcox.test(a3);wilcox.test(b1);wilcox.test(b2);wilcox.test(b3)
#a1: h.sig., a2: n. sig., a3: h. sig., b1: sig.,b2: n. sig., b3: h. sig.
# No fog precipitation can be detected in laurel forest (fog prec. could be zero),
# other 2 sites have fog prec.

# Test 2 for nets
wilcox.test(na1);wilcox.test(na2);wilcox.test(na3);wilcox.test(nb1);wilcox.test(nb2);wilcox.test(nb3)
# not significant anywhere, but the poor data can be blamed for this

#' Data from site 1 is worthless (because the rain pattern is too different due to 
#' spatial disjunction). this can be seen in the nonesense result that the forest in site 1
#' collects far less than the control sides.
#' Site 2 is more or less zero.
#' The forest from side 3 seems to be pretty efficient in fog precipitation.

# Plot der einzelnen Werte der Fog precipitation
plot(1:9,a1,col="blue",ylim=(c(-500,500)))
points(1:9,(a2),col="red")
points(1:9,(a3),col="green")
title("Plot der einzelnen Werte der Fog precipitation Day1")
#legend(x="topleft",legend=c("Mixed","Laurel","Pine"))

maxim2<-max(na.omit(prec[136:162,4]))
plot(1:9,(b1),col="blue",ylim=(c(-500,maxim2)))
points(1:9,(b2),col="red")
points(1:9,(b3),col="green")
title("Plot der einzelnen Werte der Fog precipitation Day2")

# look if the ratios stay the same between the two days
boxplot(a1,b1,a2,b2,a3,b3)
ratio1<-a1/b1
ratio2<-a2/b2
ratio3<-a3/b3
maxi<-max(na.omit(c(ratio1,ratio2,ratio3)))
mini<-min(na.omit(c(ratio1,ratio2,ratio3)))
plot(ratio1,col="blue",ylim=(c(mini,maxi))) # two outliers
points(ratio2,col="red") # huge variance (maybe because close to zero)
points(ratio3,col="green") # quite good; little variance
title("Plot of ratio between both days for all 9 devices in each site")
# look at the deviations here
#'  this is a bit strange. my explanation: key factor in site 2 is the rain,
#'  which leads to a random precipitation distribution (thus little concordance
#'  between the two days), whereas in site 3 it is fog precipitation, which depends
#'  on the needles (which stay the same) and is thus very stable over time

## mean daily precipitation per site
# isn't fog precipitation sufficient?

## mean precipitation per device over all measurement days
# isn't fog precipitation sufficient?

## mean daily precipitation in control devices per site
mean_a1;mean_a2;mean_a3;mean_b1;mean_b2;mean_b3
# huge variance between both days and sites (makes it harder to compare)

## mean daily fog precipitation per site
mean(a1);mean(a2);mean(a3);mean(b1);mean(b2);mean(na.omit(b3))
# huge variance between both days and sites (makes it harder to compare)

## mean fog precipitation per device over all measurement days
(a1+b1)/2;
(a2+b2)/2;
(na.omit(a3+b3))/2

# ToDo: ####
#' Make plots nicer




# Vertical Structure ####

vert<-read.csv("Vertical structure.csv",header=T,sep=",")
plot(vert[(1:20),3])

#"Density" of vertical structure (Number of interceptions with canopy)
mean(vert[(1:20),3])
mean(vert[(21:40),3])
mean(vert[(41:60),3])
means<-c(mean(vert[(1:20),3]),mean(vert[(21:40),3]),mean(vert[(41:60),3]))
sd(vert[(1:20),3])
sd(vert[(21:40),3])
sd(vert[(41:60),3])
sdev<-c(sd(vert[(1:20),3]),sd(vert[(21:40),3]),sd(vert[(41:60),3]))
#xpos<-barplot(means,xlab=("Location"),ylab=("Density"),cex.names  = 2,col="brown",ylim=c(0,7),yaxt="n",cex.lab=1.5,horiz=TRUE)
xpos<-barplot(means,xlab=("Location"),ylab=("Density"),cex.names  = 2,col="brown",ylim=c(0,10),xaxt="n",cex.lab=1.5)
axis(1,at=xpos,labels=seq(1,3,by=1),las=1)
#axis(2,at=xpos,labels=seq(1,3,by=1),las=2)
arrows(xpos,means - sdev,xpos,means + sdev, angle=90, length=.05, code=3) # barplot mit standardabweichung
# wie angenommen: 1laurel ,2. mixed, 3.pine (pine hat auch h�chstes sd wie angenommen)

# man k�nnte dies noch mit dem jeweiligen Niederschlag �ber den Gef��en in Verbindung setzen: prec~vert



## Deskriptive Waldbeschreibung
for1<-c(c(6.5,7,10.5,11),c(8,9:10.5),c(8:10),c(9,12),c(7,10,12,13),c(8.5,12.5,13.5),c(7,9.5,10,15,17,17.5),c(2,4,9,9.5,10.5,11),c(3,4.5:5.5,8.5,9.5,10),
        c(6,8:9.5,10.5:12.5),c(6,7:8.5,10.5,12.5,13.5,18),c(5.5,6,8.5,9,10,15.5,19),c(3,8,9.5,11,13.5,14,16),c(3,10,13,18),c(0.5,2,3.5,7.5,11,13,14,16:18.5),
        c(0.5,8,16.5,17),c(0.5,8),c(0.5,1,2,6,8:11),c(6:8.5),c(3.5,5,7,7.5))
for2<-c(c(6.5:8),c(4,5.5:7.5),c(4.5,5,7:10),c(2,5.5,10.5,11),c(2:3,5.5:7),c(2,2.5,5.5,7,8:10),c(6,6.5,8,10,10.5),c(5.5,7:8),c(4.5,7.5,8.5),
        c(4,6:8),c(6:8.5),c(4.5,5.5,7,8,8.5),c(6,7.5:9),c(7.5,8,9),c(8,8.5),c(6:10,11),c(9.5,11.5:13),c(2,3,5:6,8.5:11.5,13:14),c(4.5:7.5,10.5,11),c(2,2.5,6,7,8:9.5))
for3<-c(c(7.5,8.5,11.5),c(15:20),c(16,21:23),c(15.5,16),c(10,16,20:24),c(18,20),c(26,27.5:30.5),c(24,25.5:27),c(24.5,27),c(21,23,24,25:26.5),c(16,20,21.5,23,25.5,27,28,29.5),
        c(18,20:24),c(18.5,20.5:22),c(18,19.5,22.5:25),c(20,21.5),c(17,20.5),c(14,16,20.5,21.5:24.5),c(17),c(0),c(4.5,5:7))
#plot(sort(for2))
#plot(rep(0,length(for2)),sort(for2))
mean(for1);mean(for2);mean(for3)
sd(for1);sd(for2);sd(for3)
hist(for1,breaks=(max(for2)-min(for1))*2)
hist(for2,breaks=(max(for2)-min(for2))*2)
hist(for3,breaks=(max(for2)-min(for2))*3)
