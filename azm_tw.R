#teamwork interrater reliability

library(pastecs) #for stat.desc
library(Hmisc)
#install.packages("multilevel") #for rwg
library(multilevel)
#install.packages("ggplot2")
library(ggplot2)

setwd("~/Dropbox/PhDDropbox/team_learning/article_azm/azm_rem")

azm.att.raw<-read.csv("azm_attribute.csv", header=T, dec=".", na.strings=c("999", "888", "88", "99"))
azm.tw.raw<-azm.att.raw[,c(2,5,15:18,25:28,35:38)]
azm.onemind.raw<-azm.att.raw[,c(2,5,48:51)]
azm.onemind.raw$denkstijl4REV<- 6 - azm.onemind.raw$denkstijl4
azm.objperf<-azm.att.raw[,c(2,5,9:14,19:24,29:34)]

#CORRECT ORDER OF DATAFILE
tw.rob<-azm.tw.raw[,c(1,2,7:10)]
tw.rob[,1]<-"rob"
colnames(tw.rob)<-colnames(azm.tw.raw[,c(1:6)])
tw.pat<-azm.tw.raw[,c(1,2,11:14)]
tw.pat[,1]<-"pat"
colnames(tw.pat)<-colnames(azm.tw.raw[,c(1:6)])

azm.tw<-rbind(azm.tw.raw[,c(1:6)],tw.rob, tw.pat)

#calculate rwg
#10 point scale: 
#normal distribution:1.45; slightly skewed: 6.30, moderate skew: 5.09
rwg.twN<-rwg.j(azm.tw[,3:6], azm.tw.raw$oef_cont, ranvar=1.45)
summary(rwg.twN)
rwg.twSS<-rwg.j(azm.tw[,3:6], azm.tw.raw$oef_cont, ranvar=6.30)
summary(rwg.twSS)
rwg.twMS<-rwg.j(azm.tw[,3:6], azm.tw.raw$oef_cont, ranvar=5.09)
summary(rwg.twMS)

#plot rwg values
rwg.twN$dist<-"N"
rwg.twSS$dist<-"SS"
rwg.twMS$dist<-"MS"
rwg.tw<-rbind(rwg.twN, rwg.twSS, rwg.twMS)
#vlinecon<-data.frame(aggregate(rwg.tw$rwg.j, by=list(dist) ,mean, na.rm=TRUE))
vlinecon$thres<-0.7
azm.tw.rwg.plot<-
  ggplot(rwg.tw, aes(rwg.j, fill=as.factor(dist)))+
  geom_bar() +
  geom_vline(xintercept=0.7) +
  scale_fill_discrete(name = "Type of Distribution", labels=c("moderately skweded (MS)", 
                                                              "null distribution (N)", 
                                                              "slightly skewed (SS)")) +

  ggtitle ("Rwg of Team Work by distribution") +
  facet_wrap(~dist, as.table=TRUE)+
  #geom_vline(aes(xintercept=x),vlinecon, show.guide=TRUE)+
  annotate("text", label="mean rwg", x=0.7, y=8, size=4)
azm.tw.rwg.plot  
ggsave("RWG_tw.azm.pdf")

#ICC
mult.icc(azm.tw[,c(3:6)], azm.tw$oef_cont)

#mean teamwork
azm.tw$mean<-rowMeans(azm.tw[,c(3:6)], na.rm=T)
mean(azm.tw$mean, na.rm=T)
sd(azm.tw$mean, na.rm=T)
twmean<-aggregate(azm.tw[,7], by=list(azm.tw$oef_cont), mean, na.rm=T)
twmean$qual[twmean$x > 7.499999]<-"good"
twmean$qual[twmean$x < 5.9999]<-"bad"
twmean$qual[twmean$x > 5.9999 & twmean$x < 7.4999]<-"average"
twmean.t<-t(twmean)
twmean.t[1,]<-c(paste("MLE",1:28, sep=""))

#one mind, one thinking style
rwg.tsN<-rwg.j(azm.onemind.raw[,c(3:5,7)], azm.onemind.raw$oef_cont, ranvar=2)
summary(rwg.tsN)
rwg.tsSS<-rwg.j(azm.onemind.raw[,c(3:5,7)], azm.onemind.raw$oef_cont, ranvar=1.34)
summary(rwg.tsSS)
rwg.tsMS<-rwg.j(azm.onemind.raw[,c(3:5,7)], azm.onemind.raw$oef_cont, ranvar=0.90)
summary(rwg.tsMS)

#plot rwg values
rwg.tsN$dist<-"N"
rwg.tsSS$dist<-"SS"
rwg.tsMS$dist<-"MS"
rwg.ts<-rbind(rwg.tsN, rwg.tsSS, rwg.tsMS)
#vlinecon<-data.frame(aggregate(rwg.tw$rwg.j, by=list(dist) ,mean, na.rm=TRUE))
vlinecon$thres<-0.7
azm.ts.rwg.plot<-
  ggplot(rwg.ts, aes(rwg.j, fill=as.factor(dist)))+
  geom_bar() +
  geom_vline(xintercept=0.7) +
  scale_fill_discrete(name = "Type of Distribution", labels=c("moderately skweded (MS)", 
                                                              "null distribution (N)", 
                                                              "slightly skewed (SS)")) +
  
  ggtitle ("Rwg of Team Thinking Style by distribution") +
  facet_wrap(~dist, as.table=TRUE)+
  #geom_vline(aes(xintercept=x),vlinecon, show.guide=TRUE)+
  annotate("text", label="mean rwg", x=0.7, y=8, size=4)
azm.ts.rwg.plot  
ggsave("RWG_ts.azm.pdf")

#ICC
mult.icc(azm.onemind.raw[,c(3:5,7)], azm.onemind.raw$oef_cont)

#mean team thinking style
azm.onemind.raw$mean<-rowMeans(azm.onemind.raw[,c(3:5,7)], na.rm=T)
mean(azm.onemind.raw$mean, na.rm=T)
sd(azm.onemind.raw$mean, na.rm=T)
tsmean<-aggregate(azm.onemind.raw[,7], by=list(azm.onemind.raw$oef_cont), mean, na.rm=T)

azm.teamvar<-cbind(twmean, tsmean)
azm.teamvar <- azm.teamvar[,-5]
colnames(azm.teamvar)[2]<-"team.perfo"
colnames(azm.teamvar)[5]<-"thinkstyle"

write.csv(twmean.t, "twmean.csv")
write.csv(azm.teamvar, "azmteamvar.csv")


###IRR for objective performance
azm.objperf$Peermean<-rowMeans(azm.objperf[,4:8], na.rm=T)
azm.objperf$Patmean<-rowMeans(azm.objperf[,16:20], na.rm=T)
azm.objperf$Robmean<-rowMeans(azm.objperf[,10:14], na.rm=T)
plot(azm.objperf[,21:23])
summary(azm.objperf[,c(3,9, 15, 21:23)])

#calculate rwg for mean
#10 point scale: 
#normal distribution:1.45; slightly skewed: 6.30, moderate skew: 5.09
rwg.perfN<-rwg.j(azm.objperf[,21:23], azm.objperf$oef_cont, ranvar=1.45)
summary(rwg.perfN)
rwg.perfSS<-rwg.j(azm.objperf[,21:23], azm.objperf$oef_cont, ranvar=6.30)
summary(rwg.perfSS)
rwg.perfMS<-rwg.j(azm.objperf[,21:23], azm.objperf$oef_cont, ranvar=5.09)
summary(rwg.perfMS)

#rwg for total score by grader
summary(rwg.perfGrade.N<-rwg.j(azm.objperf[,c(3,9,15)], azm.objperf$oef_cont, ranvar=1.45))

#rwg score for A to E step (Null distribution) 
summary(rwg.perfA.N<-rwg.j(azm.objperf[,c(4,10,16)], azm.objperf$oef_cont, ranvar=1.45))
summary(rwg.perfB.N<-rwg.j(azm.objperf[,c(5,12,17)], azm.objperf$oef_cont, ranvar=1.45))
summary(rwg.perfC.N<-rwg.j(azm.objperf[,c(6,13,18)], azm.objperf$oef_cont, ranvar=1.45))
summary(rwg.perfD.N<-rwg.j(azm.objperf[,c(7,14,19)], azm.objperf$oef_cont, ranvar=1.45))
summary(rwg.perfE.N<-rwg.j(azm.objperf[,c(8,15,20)], azm.objperf$oef_cont, ranvar=1.45))

#rwg score for A to E step (MS distribution) 
summary(rwg.perfA.MS<-rwg.j(azm.objperf[,c(4,10,16)], azm.objperf$oef_cont, ranvar=5.09))
summary(rwg.perfB.MS<-rwg.j(azm.objperf[,c(5,12,17)], azm.objperf$oef_cont, ranvar=5.09))
summary(rwg.perfC.MS<-rwg.j(azm.objperf[,c(6,13,18)], azm.objperf$oef_cont, ranvar=5.09))
summary(rwg.perfD.MS<-rwg.j(azm.objperf[,c(7,14,19)], azm.objperf$oef_cont, ranvar=5.09))
summary(rwg.perfE.MS<-rwg.j(azm.objperf[,c(8,15,20)], azm.objperf$oef_cont, ranvar=5.09))

#rwg score for A to E step (SS distribution) 
summary(rwg.perfA.SS<-rwg.j(azm.objperf[,c(4,10,16)], azm.objperf$oef_cont, ranvar=6.30))
summary(rwg.perfB.SS<-rwg.j(azm.objperf[,c(5,12,17)], azm.objperf$oef_cont, ranvar=6.30))
summary(rwg.perfC.SS<-rwg.j(azm.objperf[,c(6,13,18)], azm.objperf$oef_cont, ranvar=6.30))
summary(rwg.perfD.SS<-rwg.j(azm.objperf[,c(7,14,19)], azm.objperf$oef_cont, ranvar=6.30))
summary(rwg.perfE.SS<-rwg.j(azm.objperf[,c(8,15,20)], azm.objperf$oef_cont, ranvar=6.30))

