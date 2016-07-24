rm(list=ls())
setwd("/Users/abbiepopa/Documents/Lab/DPTB")
e<-read.csv("everyone_demo.csv")

setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

Dx<-read.csv("DxCodes.csv")
###################
###Overall Pupil###
###################

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")

pupil<-read.csv("pupilalldata_paper1.1.csv")

colnames(Dx)[1]<-"Subj"

pupilDxWide<-merge(pupil, Dx)
pupilDxWide<-pupilDxWide[,c("Subj","AngryDil","HappyDil","Dx")]


library(reshape)

pupilDxLong<-melt(pupilDxWide,id=c("Subj","Dx"))

library(nlme)

#fit for condition and dx

fit<-lme(value~variable*Dx, random=~1|Subj, data=pupilDxLong)

###just checking that I didn't do something silly by putting it all in one model
#angry vs. happy
t.test(pupilDxWide$AngryDil, pupilDxWide$HappyDil)

#22q vs TD
t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"AngryDil"],pupilDxWide[which(pupilDxWide$Dx=="22q"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"HappyDil"],pupilDxWide[which(pupilDxWide$Dx=="22q"),"HappyDil"])

#Each Bar vs. 0

t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"HappyDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="22q"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="22q"),"HappyDil"])

#output stats for datagraph

library(psych)

stats<-describeBy(pupilDxWide[,c("AngryDil","HappyDil")], group=pupilDxWide$Dx )

statsexp<-rbind(stats$TD["AngryDil",c("mean","sd","n","se")],
	stats$'22q'["AngryDil",c("mean","sd","n","se")],
	stats$TD["HappyDil",c("mean","sd","n","se")],
	stats$'22q'["HappyDil",c("mean","sd","n","se")])
	
rownames(statsexp)<-c("TD: Angry Dil","22q: Angry Dil","TD: Happy Dil","22q: Happy Dil")
	
write.csv(statsexp,"overalpupil.csv")

#######################
###Time Course Pupil###
#######################

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")
pupilTC<-read.csv("AllTC25_pupil_paper1.1.csv")

pupilTCDx<-merge(pupilTC, Dx, by.x="Subj", by.y="CABIL_ID", all.x=T)
pupilTCDx<-merge(pupilTCDx, e, by.x="Subj", by.y="CABIL_ID", all.x=T)

pupilTCLong<-melt(pupilTCDx, id=c("Subj","Dx", "Dx_Code","Age","Gender_Code","Cluster.Analysis.","Overall.EG","Spence_Total_T_Score_Parent1","ABAS_GAC_Composite_Parent1","WISCIV_FullScale_C" ))

###so we have three things going on, 22q vs TD, Happy vs. Angry, and Begin vs. End

#Dx and Time; stratified by emotion

fita<-lme(value~variable*Dx, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),])

fith<-lme(value~variable*Dx, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),])

#Dx and Emotion; stratified by time
fitb<-lme(value~variable*Dx, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),])

fite<-lme(value~variable*Dx, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),])

###export stats for datagraph

stats<-describeBy(pupilTCDx[,c("happy_begin_dil","angry_begin_dil","happy_end_dil","angry_end_dil")], group=pupilTCDx$Dx)

#for now I'm exporting these in the order they already are in datagraph, eventually I will need to change this to make it more consistent with the other graphs in the paper; if there even ever is a paper :(
statsexp<-rbind(stats$'22q'["angry_begin_dil",c("mean","sd","n","se")],
	stats$TD["angry_begin_dil",c("mean","sd","n","se")],
	stats$'22q'["happy_begin_dil",c("mean","sd","n","se")],
	stats$TD["happy_begin_dil",c("mean","sd","n","se")],
	stats$'22q'["angry_end_dil",c("mean","sd","n","se")],
	stats$TD["angry_end_dil",c("mean","sd","n","se")],
	stats$'22q'["happy_end_dil",c("mean","sd","n","se")],
	stats$TD["happy_end_dil",c("mean","sd","n","se")])
	
rownames(statsexp)<-c("22q: Angry Begin Dil","TD: Angry Begin Dil","22q: Happy Begin Dil","TD: Happy Begin Dil","22q: Angry End Dil","TD: Angry End Dil","22q: Happy End Dil","TD: Happy End Dil")

write.csv(statsexp,"pupilTCstats.csv")

###Stats for Graph###
###Overall###

#Is dilation different from zero?
t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="22q"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"HappyDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="22q"),"HappyDil"])

#Is 22q diff from TD
t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"AngryDil"],pupilDxWide[which(pupilDxWide$Dx=="22q"),"AngryDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"HappyDil"],pupilDxWide[which(pupilDxWide$Dx=="22q"),"HappyDil"])

#Is angry diff from happy
t.test(pupilDxWide[which(pupilDxWide$Dx=="TD"),"AngryDil"],pupilDxWide[which(pupilDxWide$Dx=="TD"),"HappyDil"])

t.test(pupilDxWide[which(pupilDxWide$Dx=="22q"),"AngryDil"],pupilDxWide[which(pupilDxWide$Dx=="22q"),"HappyDil"])

###Time Course - Begin###
#Is dilation different from zero?
t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_begin_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_begin_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_begin_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_begin_dil"])

#Is 22q diff from TD
t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_begin_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_begin_dil"])

#Is angry diff from happy
t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_begin_dil"])


t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_begin_dil"])

###Time Course - End###
#Is dilation different from zero?
t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_end_dil"])

#Is 22q diff from TD
t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_end_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_end_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_end_dil"])

#Is angry diff from happy
t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_end_dil"],pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_end_dil"])


t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_end_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_end_dil"])

###Time Course: Is Begin diff from End###
t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="TD"),"angry_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"angry_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="TD"),"happy_end_dil"])

t.test(pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_begin_dil"],pupilTCDx[which(pupilTCDx$Dx=="22q"),"happy_end_dil"])

###Age###
summary(lm(angry_begin_dil~Dx+Age, data=pupilTCDx))
summary(lm(angry_end_dil~Dx+Age, data=pupilTCDx))
summary(lm(happy_begin_dil~Dx+Age, data=pupilTCDx))
summary(lm(happy_end_dil~Dx+Age, data=pupilTCDx))

summary(lme(value~variable*Dx+variable*Age, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),]))

summary(lme(value~variable*Dx+variable*Age, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),]))

summary(lme(value~variable*Dx+variable*Age, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),]))

summary(lme(value~variable*Dx+variable*Age, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),]))

###Gender###
summary(lm(angry_begin_dil~Dx+as.factor(Gender_Code), data=pupilTCDx))
summary(lm(angry_end_dil~Dx+as.factor(Gender_Code), data=pupilTCDx))
summary(lm(happy_begin_dil~Dx+as.factor(Gender_Code), data=pupilTCDx))
summary(lm(happy_end_dil~Dx+as.factor(Gender_Code), data=pupilTCDx))

summary(lme(value~variable*Dx+variable*as.factor(Gender_Code), random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),]))

summary(lme(value~variable*Dx+variable*as.factor(Gender_Code), random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),]))

summary(lme(value~variable*Dx+variable*as.factor(Gender_Code), random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),]))

summary(lme(value~variable*Dx+variable*as.factor(Gender_Code), random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),]))

###FSIQ###
summary(lm(angry_begin_dil~Dx+WISCIV_FullScale_C, data=pupilTCDx, na.action=na.omit))
summary(lm(angry_end_dil~Dx+WISCIV_FullScale_C, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_begin_dil~Dx+WISCIV_FullScale_C, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_end_dil~Dx+WISCIV_FullScale_C, data=pupilTCDx,  na.action=na.omit))

summary(lme(value~variable*Dx+variable*WISCIV_FullScale_C, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*WISCIV_FullScale_C, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*WISCIV_FullScale_C, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),],  na.action=na.omit))

summary(lme(value~variable*Dx+variable*WISCIV_FullScale_C, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))


###SCAS###
summary(lm(angry_begin_dil~Dx+Spence_Total_T_Score_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(angry_end_dil~Dx+Spence_Total_T_Score_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_begin_dil~Dx+Spence_Total_T_Score_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_end_dil~Dx+Spence_Total_T_Score_Parent1, data=pupilTCDx,  na.action=na.omit))

summary(lme(value~variable*Dx+variable*Spence_Total_T_Score_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*Spence_Total_T_Score_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*Spence_Total_T_Score_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),],  na.action=na.omit))

summary(lme(value~variable*Dx+variable*Spence_Total_T_Score_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))

###ABAS###
summary(lm(angry_begin_dil~Dx+ABAS_GAC_Composite_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(angry_end_dil~Dx+ABAS_GAC_Composite_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_begin_dil~Dx+ABAS_GAC_Composite_Parent1, data=pupilTCDx, na.action=na.omit))
summary(lm(happy_end_dil~Dx+ABAS_GAC_Composite_Parent1, data=pupilTCDx,  na.action=na.omit))

summary(lme(value~variable*Dx+variable*ABAS_GAC_Composite_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_begin_dil"), which(pupilTCLong$variable == "angry_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*ABAS_GAC_Composite_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "happy_begin_dil"), which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))

summary(lme(value~variable*Dx+variable*ABAS_GAC_Composite_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable=="angry_begin_dil"),which(pupilTCLong$variable == "happy_begin_dil")),],  na.action=na.omit))

summary(lme(value~variable*Dx+variable*ABAS_GAC_Composite_Parent1, random=~1|Subj, data=pupilTCLong[c(which(pupilTCLong$variable == "angry_end_dil"),which(pupilTCLong$variable == "happy_end_dil")),], na.action=na.omit))


