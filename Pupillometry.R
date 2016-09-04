### IMPORT DATA
#setwd("/Users/ampopa/Desktop/Pupillometry")
#pupil788 <- read.table("pupil788.csv", header=TRUE, sep=",")

#pupil788 <- read.table(file.choose(),header=TRUE,sep=",")

setwd("/Users/abbiepopa/Documents/Lab/DPTB/ParticipantData")
pupil788 <-read.csv("757.csv")

### Trim to useful rows and columns          
columns <-c("Subject","DiameterPupilLeftEye","DiameterPupilRightEye","ProcCode","Display")
pupil788col <- pupil788[columns]
pupil788face <- subset(pupil788col, Display=="FacePair" & DiameterPupilLeftEye!=-1 & DiameterPupilRightEye!=-1)

### Organize by emotion
pupil788angry <- subset(pupil788face, ProcCode=="NAI" | ProcCode=="ANI" | ProcCode=="NAV" | ProcCode=="ANV")
pupil788neutral <- subset(pupil788face, ProcCode=="NNL" | ProcCode=="NNR")
pupil788happy <- subset(pupil788face, ProcCode=="HNV" | ProcCode=="NHV" | ProcCode== "HNI" | ProcCode=="NHI")           
 
### Trim uneccesary columns and rename remaining columns
columns2 <- c("Subject","DiameterPupilLeftEye","DiameterPupilRightEye")
pupil788angry <- pupil788angry[columns2]
pupil788happy <- pupil788happy[columns2]
pupil788neutral <- pupil788neutral[columns2]
colnames(pupil788happy) <- c("Subject","DiameterPupilLeftEyeHappy","DiameterPupilRightEyeHappy")
colnames(pupil788neutral) <- c("Subject","DiameterPupilLeftEyeNeutral","DiameterPupilRightEyeNeutral")
colnames(pupil788angry) <- c("Subject","DiameterPupilLeftEyeAngry","DiameterPupilRightEyeAngry")

### Merge into one table

cbindPad <- function(...){
     args <- list(...)
     n <- sapply(args,nrow)
     mx <- max(n)
     pad <- function(x, mx){
         if (nrow(x) < mx){
             nms <- colnames(x)
             padTemp <- matrix(NA,mx - nrow(x), ncol(x))
             colnames(padTemp) <- nms
             return(rbind(x,padTemp))
         }
         else{
             return(x)
         }
     }
     rs <- lapply(args,pad,mx)
     return(do.call(cbind,rs))
 }
 pupil788data <- cbindPad(pupil788neutral, pupil788happy, pupil788angry)
 
 numrowhappy<-nrow(pupil788happy)
 numrowneutral<-nrow(pupil788neutral)
 numrowangry<-nrow(pupil788angry)
 
 happybegin<-pupil788happy[1:(numrowhappy/4),]
 neutralbegin<-pupil788neutral[1:(numrowneutral/4),]
 angrybegin<-pupil788angry[1:(numrowangry/4),]
 
 happyend<-pupil788happy[(numrowhappy-(numrowhappy/4)):numrowhappy,]
 neutralend<-pupil788neutral[(numrowneutral-(numrowneutral/4)):numrowneutral,]
 angryend<-pupil788angry[(numrowangry-(numrowangry/4)):numrowangry,]
 

### Get Means and Perform T-Tests

v<-colMeans(pupil788data, na.rm = TRUE)

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")
pupil<-read.csv("AllPupilDataPrelim.csv")
pupil$Left.Neutral<-as.numeric(as.character(pupil$Left.Neutral))
pupil$Right.Neutral<-as.numeric(as.character(pupil$Right.Neutral))
pupil$Left.Happy<-as.numeric(as.character(pupil$Left.Happy))
pupil$Right.Happy<-as.numeric(as.character(pupil$Right.Happy))
pupil$Left.Angry<-as.numeric(as.character(pupil$Left.Angry))
pupil$Right.Angry<-as.numeric(as.character(pupil$Right.Angry))

newrow<-c(pupil788$Subject[1],v[[2]],v[[3]],v[[5]],v[[6]],v[[8]],v[[9]])

pupil<-rbind(pupil,newrow)

pupil$NeutralAverage<-(pupil$Left.Neutral + pupil$Right.Neutral)/2
pupil$HappyAverage<-(pupil$Left.Happy + pupil$Right.Happy)/2
pupil$AngryAverage<-(pupil$Left.Angry + pupil$Right.Angry)/2

pupil$AngryDil <- pupil$AngryAverage - pupil$NeutralAverage
pupil$HappyDil <- pupil$HappyAverage - pupil$NeutralAverage

library(psych)

setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
Dx<-read.csv("DxCodes.csv")

colnames(Dx)<-c("Subj","Dx")

pupilDx<-merge(pupil,Dx)

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")

write.csv(pupil,"pupilalldata.csv")
write.csv(pupilDx,"pupilDx.csv")

pupilDxstats<-describeBy(pupilDx, group=pupilDx$Dx)

pupiloutstats<-rbind(pupilDxstats$TD["AngryDil",c("mean","sd","n","se")],pupilDxstats$'22q'["AngryDil",c("mean","sd","n","se")],pupilDxstats$TD["HappyDil",c("mean","sd","n","se")],pupilDxstats$'22q'["HappyDil",c("mean","sd","n","se")])

rownames(pupiloutstats)<-c("AngryDil_TD","AngryDil_22q","HappyDil_TD","HappyDil_22q")

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")
write.csv(pupiloutstats,"pupilstats.csv")

###Change

# # 

pupilchange<-read.csv("AllPupilChangeDataPrelim.csv", na.strings=".")
newrowc<-c(
colMeans(neutralbegin)[[1]],
colMeans(neutralbegin)[[2]],
colMeans(neutralbegin)[[3]],
colMeans(happybegin)[[2]],colMeans(happybegin)[[3]],
colMeans(angrybegin)[[2]],colMeans(angrybegin)[[3]],
colMeans(neutralend)[[2]],colMeans(neutralend)[[3]],
colMeans(happyend)[[2]],colMeans(happyend)[[3]],
colMeans(angryend)[[2]],colMeans(angryend)[[3]])

pupilchange<-rbind(pupilchange, newrowc)

neutral_begin_avg<-(pupilchange$Left.Neutral.Begin + pupilchange$Right.Neutral.Begin)/2

happy_begin_avg<-(pupilchange$Left.Happy.Begin + pupilchange$Right.Happy.Begin)/2

angry_begin_avg<-(pupilchange$Left.Angry.Begin + pupilchange$Right.Angry.Begin)/2

neutral_end_avg<-(pupilchange$Left.Neutral.End + pupilchange$Right.Neutral.End)/2

happy_end_avg<-(pupilchange$Left.Happy.End + pupilchange$Right.Happy.End)/2

angry_end_avg<-(pupilchange$Left.Angry.End + pupilchange$Right.Angry.End)/2

happy_begin_dil<-happy_begin_avg - neutral_begin_avg
angry_begin_dil<-angry_begin_avg - neutral_begin_avg

happy_end_dil <- happy_end_avg - neutral_end_avg
angry_end_dil <- angry_end_avg - neutral_end_avg

DilValues<-data.frame(pupilchange$Subj, happy_begin_dil, angry_begin_dil, happy_end_dil, angry_end_dil)

write.csv(DilValues, "AllTC25_pupil.csv")

colnames(DilValues)[1]<-"Subj"

DilValuesDx<-merge(DilValues, Dx)

write.csv(DilValuesDx, "AllTC25Dx_pupil.csv")

PC_Stats<-describeBy(DilValuesDx, group=DilValuesDx$Dx)

PC_out<-rbind(PC_Stats$'22q'["angry_begin_dil",c("mean","sd","n","se")],PC_Stats$TD["angry_begin_dil",c("mean","sd","n","se")],PC_Stats$'22q'["happy_begin_dil",c("mean","sd","n","se")],PC_Stats$TD["happy_begin_dil",c("mean","sd","n","se")],PC_Stats$'22q'["angry_end_dil",c("mean","sd","n","se")],PC_Stats$TD["angry_end_dil",c("mean","sd","n","se")],PC_Stats$'22q'["happy_end_dil",c("mean","sd","n","se")],PC_Stats$TD["happy_end_dil",c("mean","sd","n","se")])

rownames(PC_out)<-c("Ang_Beg_22q","Ang_Beg_TD","Hap_Beg_22q","Hap_Beg_TD","Ang_End_22q","Ang_End_TD","Hap_End_22q","Hap_End_TD")

write.csv(PC_out, "PupilChange25_Stats.csv")

# neutralbeginmeans<-rbind(neutralbeginmeans,colMeans(neutralbegin))
# happybeginmeans<-rbind(happybeginmeans,colMeans(happybegin))
# angrybeginmeans<-rbind(angrybeginmeans,colMeans(angrybegin))

# neutralendmeans<-rbind(neutralendmeans,colMeans(neutralend))
# happyendmeans<-rbind(happyendmeans,colMeans(happyend))
# angryendmeans<-rbind(angryendmeans,colMeans(angryend))

# neutralbeginmeans
# happybeginmeans
# angrybeginmeans
# neutralendmeans
# happyendmeans
# angryendmeans


# # setwd("/Users/ampopa/Desktop/Pupillometry")
# write.table(neutralbeginmeans, file="NeutralBegin.csv",sep=",",col.names=TRUE)
# write.table(happybeginmeans, file="HappyBegin.csv",sep=",",col.names=TRUE)
# write.table(angrybeginmeans, file="AngryBegin.csv",sep=",",col.names=TRUE)
# write.table(neutralendmeans, file="NeutralEnd.csv",sep=",",col.names=TRUE)
# write.table(happyendmeans, file="HappyEnd.csv",sep=",",col.names=TRUE)
# write.table(angryendmeans, file="AngryEnd.csv",sep=",",col.names=TRUE)



