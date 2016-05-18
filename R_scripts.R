# La Palma

#### Precipitation
setwd("D:/Global Change Ecology/Summer_Winter Schools/La Palma/Results")

prec<-read.csv("Data_Sampling_sheet_devices_noNA.csv",header=T,sep=";")

tapply(X=prec$X.2,INDEX=prec$X.6,FUN=mean)
str(prec)
plot(1:27,prec[85:111,4])

maxim<-max(as.numeric(prec[82:108,4]))

control<-read.csv("Control19200316.csv",header=F,sep=";")
mean_a1<-mean(control[1:3,2])
mean_a2<-mean(control[4:6,2])
mean_a3<-mean(control[7:9,2])

mean_b1<-mean(control[10:12,2])
mean_b2<-mean(control[13:15,2])
mean_b3<-mean(control[16:18,2])
# mean(control[10:18,2])

g<-(prec[84:92,4])-mean_a1

?tapply

#1.2.2 ; 2.2.3

a1<-(prec[82:90,4]-mean_a1)
a2<-(prec[91:99,4]-mean_a2)
a3<-(prec[100:108,4]-mean_a3)
b1<-(prec[109:117,4]-mean_b1)
b2<-(prec[118:126,4]-mean_b2)
b3<-(prec[127:135,4]-mean_b3)

m1_19<-mean(a1)
m2_19<-mean(a2)
m3_19<-mean(a3)
m1_20<-mean(b1)
m2_20<-mean(b2)
m3_20<-mean(b3)

barplot(c(m1_19,m2_19,m3_19))
barplot(c(m1_20,m2_20,m3_20))


plot(1:9,a1,col="blue",ylim=(c(-500,500)))
points(1:9,(a2),col="red")
points(1:9,(a3),col="green")
#legend(x="topleft",legend=c("Mixed","Laurel","Pine"))
#?legend

#prec[,4]<-as.numeric(prec[,4])
  #        prec[133,4] <-632
maxim2<-max(as.numeric(prec[109:135,4]))
plot(1:9,(b1),col="blue",ylim=(c(-500,maxim2)))
points(1:9,(b2),col="red")
points(1:9,(b3),col="green")




##### Vertical Strucutre ##

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
# wie angenommen: 1laurel ,2. mixed, 3.pine (pine hat auch höchstes sd wie angenommen)

# man könnte dies noch mit dem jeweiligen Niederschlag über den Gefäßen in Verbindung setzen: prec~vert



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
