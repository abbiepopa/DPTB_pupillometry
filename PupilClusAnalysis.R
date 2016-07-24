setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

Clus<-read.csv("ClusCodes.csv")
###################
###Overall Pupil###
###################

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")

pupil<-read.csv("pupilalldata_paper1.1.csv")

pupilClusWide<-merge(pupil, Clus)
pupilClusWide$Cluster<-as.factor(pupilClusWide$Cluster)

library(reshape)

pupilClusLong<-melt(pupilClusWide[,c("Subj","AngryDil","HappyDil","Cluster")],id=c("Subj","Cluster"))

library(nlme)

#fit for condition and dx

fit<-lme(value~variable*Cluster, random=~1|Subj, data=pupilClusLong)

#output stats for datagraph

library(psych)

stats<-describeBy(pupilClusWide[,c("AngryDil","HappyDil")], group=pupilClusWide$Cluster)

statsexp<-rbind(stats$'2'["AngryDil",c("mean","sd","n","se")],
	stats$'1'["AngryDil",c("mean","sd","n","se")],
	stats$'2'["HappyDil",c("mean","sd","n","se")],
	stats$'1'["HappyDil",c("mean","sd","n","se")])
	
write.csv(statsexp, "overalpupil_cluster.csv")

#######################
###Time Course Pupil###
#######################

pupilTC<-read.csv("AllTC25_pupil_paper1.1.csv")

pupilTCClus<-merge(pupilTC, Clus)

pupilTCClusLong<-melt(pupilTCClus, id=c("Subj","Cluster"))

#Clus and Time; stratified by emotion

fita<-lme(value~variable*Cluster, random=~1|Subj, data=pupilTCClusLong[c(which(pupilTCClusLong$variable == "angry_begin_dil"), which(pupilTCClusLong$variable == "angry_end_dil")),])
#cluster p=0.05
#interaction p=0.09

fith<-lme(value~variable*Cluster, random=~1|Subj, data=pupilTCClusLong[c(which(pupilTCClusLong$variable == "happy_begin_dil"), which(pupilTCClusLong$variable == "happy_end_dil")),])
#interaction p=0.06

#Dx and Emotion; stratified by time

fitb<-lme(value~variable*Cluster, random=~1|Subj, data=pupilTCClusLong[c(which(pupilTCClusLong$variable == "angry_begin_dil"), which(pupilTCClusLong$variable == "happy_begin_dil")),])

fite<-lme(value~variable*Cluster, random=~1|Subj, data=pupilTCClusLong[c(which(pupilTCClusLong$variable == "angry_end_dil"), which(pupilTCClusLong$variable == "happy_end_dil")),])
#emotion p=0.09

#export stats for datagraph

stats<-describeBy(pupilTCClus[,c("happy_begin_dil","angry_begin_dil","happy_end_dil","angry_end_dil")], group=pupilTCClus$Cluster)

statsexp<-rbind(stats$'1'["angry_begin_dil",c("mean","sd","n","se")],
	stats$'2'["angry_begin_dil",c("mean","sd","n","se")],
	stats$'1'["happy_begin_dil",c("mean","sd","n","se")],
	stats$'2'["happy_begin_dil",c("mean","sd","n","se")],
	stats$'1'["angry_end_dil",c("mean","sd","n","se")],
	stats$'2'["angry_end_dil",c("mean","sd","n","se")],
	stats$'1'["happy_end_dil",c("mean","sd","n","se")],
	stats$'2'["happy_end_dil",c("mean","sd","n","se")])

rownames(statsexp)<-c("Struggler: Angry Begin Dil","Coper: Angry Begin Dil","Struggler: Happy Begin Dil","Coper: Happy Begin Dil","Struggler: Angry End Dil","Coper: Angry End Dil","Struggler: Happy End Dil","Coper: Happy End Dil")

write.csv(statsexp, "pupilTCClus_Stats.csv")

###Stats for Graph###
###Overall###

#Is dilation different from zero?
t.test(pupilClusWide[which(pupilClusWide$Cluster=="2"),"AngryDil"])

t.test(pupilClusWide[which(pupilClusWide$Cluster=="1"),"AngryDil"])

t.test(pupilClusWide[which(pupilClusWide$Cluster=="2"),"HappyDil"])

t.test(pupilClusWide[which(pupilClusWide$Cluster=="1"),"HappyDil"])

#Is Coper diff from Struggler
t.test(pupilClusWide[which(pupilClusWide$Cluster=="2"),"AngryDil"], pupilClusWide[which(pupilClusWide$Cluster=="1"),"AngryDil"])

t.test(pupilClusWide[which(pupilClusWide$Cluster=="2"),"HappyDil"], pupilClusWide[which(pupilClusWide$Cluster=="1"),"HappyDil"])

#Is angry diff from Happy
t.test(pupilClusWide[which(pupilClusWide$Cluster=="2"),"AngryDil"],pupilClusWide[which(pupilClusWide$Cluster=="2"),"HappyDil"])

t.test(pupilClusWide[which(pupilClusWide$Cluster=="1"),"AngryDil"],pupilClusWide[which(pupilClusWide$Cluster=="1"),"HappyDil"])

###Time Course - Begin###
###Is dilation different from zero?
t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_begin_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_begin_dil"])
#p=0.10

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_begin_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_begin_dil"])


#Is Coper diff from Struggler
t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_begin_dil"], pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_begin_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_begin_dil"],pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_begin_dil"])

#Is angry diff from Happy
t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_begin_dil"], pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_begin_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_begin_dil"], pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_begin_dil"])

###Time Course - End###
###Is dilation different from zero?
t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_end_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_end_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_end_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_end_dil"])
#p=0.04 - happy end coper

#Is Coper diff from Struggler
t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_end_dil"], pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_end_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_end_dil"],pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_end_dil"])
#p=0.053

#Is angry diff from Happy
t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_end_dil"], pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_end_dil"])

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_end_dil"], pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_end_dil"])

### Time Course: Is begin diff from end###
t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_begin_dil"],pupilTCClus[which(pupilTCClus$Cluster=="1"),"angry_end_dil"])
#p=0.03
#copers

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_begin_dil"],pupilTCClus[which(pupilTCClus$Cluster=="2"),"angry_end_dil"])


t.test(pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_begin_dil"],pupilTCClus[which(pupilTCClus$Cluster=="1"),"happy_end_dil"])
#p=0.031
#copers

t.test(pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_begin_dil"],pupilTCClus[which(pupilTCClus$Cluster=="2"),"happy_end_dil"])

