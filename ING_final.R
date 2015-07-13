#july 12 clean up script once received clean datafile from Louisa
#set up github and commit files to it

library(informR)
teamraw<-read.csv("~/Dropbox/ed/ed_teams.csv", header=T, dec=".")
teamraw<-teamraw[,c(5,7,9)]
#Correct timing, which is currently read in as a factor
# and adjust for non-simultaneity
teamraw[,1]<-ave(teamraw[,1],teamraw$Observation,FUN=function(x) x+(1:length(x))*.001)

teamraw<-teamraw[!teamraw$Type %in% c("","handover","not in the room") ,] #delete empty rows, handovers, and not in room events
teamraw$Type<-droplevels(teamraw$Type) #drops unused level
behav<-as.character(unique(teamraw[,3]))
#info.alloc=giving info w/out being asked/ordering, giveInfOrder
#info.ret=askInfo, ask info/q
#reply = reply
#coconstr=summarizing, summarize & thinking out loud
#decision = making a decision
behav.grp<-c("health", "ia", "reply", "ir", "nonverbal",
            "coconstr", "nonverbal", "health", "health","coconstr", 
             "kwcreation", "reply_ir")
behav.table<-matrix(c(behav, behav.grp), nrow=length(behav))
for(i in 1:nrow(teamraw)){
  c<-as.character(teamraw[i,3])#retrieve type of interaction for i
  d<-behav.table[behav.table[,1] %in% c,2]#get new name for c
  teamraw[i,4]<-d}

#put team in correct order to include into eventlist
team<-teamraw[,c(4,1,2)]
#subset: take Jasmijn or Louisa's coding
team<-subset(team, grepl("_J", teamraw$Observation)) # 2 people coded the dataset for reliability
team$Observation<-droplevels(team$Observation)
team$Observation<-as.numeric(gsub("\\_J","",as.character(team$Observation))) #delete the J from the list to end up with numbers for each team

evlall<-gen.evl(team, null.events=c("health"))
names(evlall)
evlall$event.key
evlall$null.event
head(evlall$eventlist$"9_J")

#create intecepts only model. must set contr to FALSE if using interval data
#type indicates location of statlist. 1 is global (homogenous for all teams), 2 is local
evlall.ints<-gen.intercepts(evlall, basecat="ia", type=1, contr=T)
#error invalid 'times' values. because I didn't changed the null event name from the old name of health_reg etc to the new name.
#i also forgot to change the basecat from "infoalloc" to "ia"
lapply(evlall$eventlist,function(x) which(diff(x[,2])<=0)) #shows where there are problems in the eventlist

rndupes<-rownames(cbind(unlist(lapply(evlall$eventlist,function(x) which(diff(x[,2])<=0)))+1))
team.split<-split(team, team$Observation)
team.split.ss<-team.split[which(names(team.split)%in%rndupes)]
tmp.d1<-cbind(unlist(lapply(evlall$eventlist,function(x) which(diff(x[,2])<=0))))

tmp.d2<-cbind(1,tmp.d1)[match(names(team.split.ss),rownames(cbind(1,tmp.d1))),]
for(i in 1:length(team.split.ss)){
  team.split.ss[[i]]<-team.split.ss[[i]][tmp.d2[i,1]:tmp.d2[i,2],]
}
team.split[which(names(team.split)%in%rndupes)]<-team.split.ss
team<-do.call("rbind",team.split)
#save(team,file="~/Dropbox/PhDDropbox/team_learning/teamFixed.Rdata")
#check team
str(team)

#12 July recreate teh eventlist based on the team dataset without the double entries.
evlall<-gen.evl(team, null.events=c("health"))
names(evlall)
evlall$event.key
evlall$null.event
head(evlall$eventlist$"9_J")

#create intecepts only model. must set contr to FALSE if using interval data
#type indicates location of statlist. 1 is global (homogenous for all teams), 2 is local
evlall.ints<-gen.intercepts(evlall, basecat="ia", type=1, contr=T)

summary(fit1<-rem(evlall$eventlist, evlall.ints, estimator="MLE"))
#sim events --> solved: problems in ed-team. entries are double
#if contr=F, error
#all communication ties, excpet replying occur sig less often than info allocation

#h1: persistence of info alloc, coconstrc
inertia.events<-paste(evlall$event.key[c(2,5),1],evlall$event.key[c(2,5),1],sep="")
Int.sforms<-gen.sformlist(evlall, inertia.events) #generate sform statistics
#add interia events to intercepts (fixed effects) statlist
FEIn<-slbind(Int.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) #order of arguments:sform FOLLOWEd by statlist
summary(fit2<-rem(evlall$eventlist, FEIn, estimator="MLE"))

#H2a:persistence info alloc --> info reti
h2a<-paste("b+f")
h2a.sforms<-gen.sformlist(evlall, h2a)
h2aeffect<-slbind(h2a.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h2aeffect, estimator="MLE"))

#h2b: info reti --> reply ---> info alloc
h2b<-paste("fcb")
h2b.sforms<-gen.sformlist(evlall, h2b)
h2beffect<-slbind(h2b.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h2beffect, estimator="MLE"))

#h2c: info reti --> reply_ir ---> info alloc
h2c<-paste("fhb")
h2c.sforms<-gen.sformlist(evlall, h2c)
h2ceffect<-slbind(h2c.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h2ceffect, estimator="MLE"))

#h3: persistent co-constru-->kw creation
h3<-paste("d+g")
h3.sforms<-gen.sformlist(evlall, h3)
h3effect<-slbind(h3.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h3effect, estimator="MLE"))

#########################
h4.1<-paste(c("b+fcd+g"))
h4.1.sforms<-gen.sformlist(evlall, h4.1)
h4.1effect<-slbind(h4.1.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h4.1effect, estimator="MLE"))

h4.2<-paste("b+fhd+g")
h4.2.sforms<-gen.sformlist(evlall, h4.2)
h4.2effect<-slbind(h4.2.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h4.2effect, estimator="MLE"))

h4.3<-paste("fcb+d+g")
h4.3.sforms<-gen.sformlist(evlall, h4.3)
h4.3effect<-slbind(h4.3.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h4.3effect, estimator="MLE"))

h4.4<-paste("fhb+d+g")
h4.4.sforms<-gen.sformlist(evlall, h4.4)
h4.4effect<-slbind(h4.4.sforms, evlall.ints, new.names=T, event.key=evlall$event.key) 
summary(rem(evlall$eventlist, h4.4effect, estimator="MLE"))

###############################
azm.teamvar<-read.csv("~/Dropbox/PhDDropbox/team_learning/article_azm/azm_rem/azmteamvar.csv", header=T, dec=".")

qual<-NA
think<-NA
for(i in 1:nrow(azm.teamvar)){
  grp.nbr <- azm.teamvar[i,5]
  twqual <- azm.teamvar[i,4]
  thinklevel <- azm.teamvar[i,6]
  qual[team$Observation == grp.nbr] <- twqual
  think[team$Observation == grp.nbr] <- thinklevel
}
team<-cbind(team, qual, think)

think.evs<-unlist(glapply(team$think, team$Observation, unique, regroup=F))
think.evs[which(is.na(think.evs))]<-3.71
think.evs <- ifelse(think.evs > 3.71,1,0)
kwcreatTS.1<-slbind.cond(think.evs,h4.4effect, sl.ind=5, var.suffix="thinkstyle" )
kwcreatTS.2<-slbind.cond(think.evs,h4.4effect, sl.ind=1:5, var.suffix="thinkstyle" )
summary(rem(evlall$eventlist, kwcreatTS.1, estimator="MLE"))
fitthink<-rem(evlall$eventlist, kwcreatTS.2, estimator="MLE")
#If the team has a common thinking style, kw creation chain begins with informaiton allocation (but only marginally significnat)
#if the team has no common thinking style, kw creation begins with information retrieval
#if the team has a common thinking style, it is less likely to begin it's kw cration chain with informaiton allocation. 

perfo.evs<-unlist(glapply(team$qual, team$Observation, unique, regroup=F))
perfo.evs[21]<-median(perfo.evs, na.rm=T)
good.evs<-ifelse(perfo.evs == 3, 1,0)
kwcreatQual.1<-slbind.cond(good.evs,h4.4effect, sl.ind=5, var.suffix="good" )
kwcreatQual.2<-slbind.cond(good.evs,h4.4effect, sl.ind=1:5, var.suffix="good" )
summary(rem(evlall$eventlist, kwcreatQual.1, estimator="MLE"))
fitperfo<-rem(evlall$eventlist, kwcreatQual.2, estimator="MLE")
#if the team is performing less than good, kw creation begins with information retrieval


###############################
#liklihood of events occuring
#h1coef<-rem(evlall$eventlist, FEIn, estimator="MLE")$coef
#likihood pers coconst 
#ccCChaz<-exp(sum(h1coef[c(3,6)]))
#all0h1Haz<-exp(sum(h1coef[(1:4)[-3]]))
#round(ccCChaz/sum(ccCChaz + all0h1Haz),2)
#88 % chance of co-construction to be followed by another co-construction event

#h2acoef<-rem(evlall$eventlist, h2aeffect, estimator="MLE")$coef
#iairHaz<-exp(sum(h2acoef[c(2, 5)]))
#all0h2aHaz<-exp(sum(h2acoef[(1:4)[-2]]))
#round(iairHaz/sum(iairHaz + all0h2aHaz),2) #not sure if this is correct...

#h2bcoef<-rem(evlall$eventlist, h2beffect, estimator="MLE")$coef
#h3coef<-rem(evlall$eventlist, h3effect, estimator="MLE")$coef
#h4coef<-rem(evlall$eventlist, h4effect, estimator="MLE")$coekf
