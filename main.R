library(plyr)
library(Hmisc)
library(rgl)
library(expm)
library(dplyr)
library(beepr)
library(qualityTools)
library(ggplot2)




getExperiment(authority.v=c(T),ticks=1000,composability.v=0.240,m.v=150,f.v=c(1.00),turbulence.v=c(0),release.v=c(1),size=100,repetitions=10,seed=280719,plot=F) # round(runif(1,1,10000),0)

d10.01 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F) 
d10.02 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=13012010,plot=F) 
d10.03 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=16012014,plot=F)
d10.04 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F) 
d10.05 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.080,0.084,0.001),seq(0.086,0.089,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F) 
d10.06 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.116,0.119,0.001),seq(0.121,0.124,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F)
d10.09 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.116,0.119,0.001),seq(0.121,0.124,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=13012010,plot=F)
d10.12 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.116,0.119,0.001),seq(0.121,0.124,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=075,seed=16012014,plot=F)
d10.13 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.116,0.119,0.001),seq(0.121,0.124,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=050,seed=05032018,plot=F)
d10.07 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.126,0.129,0.001),seq(0.131,0.134,0.001)),m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F)
d10.08 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.136,0.139,0.001))                       ,m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F)
d10.10 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.136,0.139,0.001))                       ,m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=075,seed=13012010,plot=F)
d10.11 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.131,0.134,0.001))                       ,m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=075,seed=16012014,plot=F)
d10.21 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.080,0.090,0.001))                       ,m.v=c(100)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=050,seed=26012014,plot=F)

d10.20 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(050)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F)
d10.15 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(025)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F)
d10.16 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(075)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F) 
d10.17 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.190,0.005)),m.v=c(125)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F)
d10.18 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(150)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F)
d10.19 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.175,0.250,0.005)),m.v=c(150)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F) 

d10.14 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.170,0.005)),m.v=c(050)  ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F)

d10 <- rbind(d10.01,d10.02,d10.03,d10.04,d10.05,d10.06,d10.07,d10.08,d10.09,d10.10,d10.11,d10.12,d10.13,d10.15,d10.16,d10.17,d10.18,d10.19,d10.20,d10.21)

## write.table(d10,"data.csv",sep=",") ## Simulations are very time consuming, therefore, here is a a dump, which is also submitted.

d10.agr <- ddply(d10,c("compo","m","authority","turbulence","release"),summarise
                 ,size                 =mean(size)
                 ,i                    =mean(i)
                 ,ticks                =mean(ticks)
                 ,compl                =mean(completeness,na.rm=T)
                 ,ticks.at80perc       =mean(ticksTurned80perc,na.rm=T)
                 ,ticks.at80perc.var   =var(ticksTurned80perc,na.rm=T)
                 ,effica               =mean(effica,na.rm=T)
                 ,dircC                =mean(directCouplingCost,na.rm=T) 
                 ,propC                =mean(propagationCost,na.rm=T)    
                 ,effica.cum           =mean(effica.cum,na.rm=T)   #/ mean(ticks)
                 ,complete.cum         =mean(complete.cum,na.rm=T) #/ mean(ticks)
                 ,dirCoupl.50perc      =mean(directCouplCost.at50perc,na.rm=T)
                 ,inddirCoupl.50perc   =mean(propagationCost.at50perc,na.rm=T) 
                 ,ic1st                  =mean(ic1st)
                 ,ic2nd                  =mean(ic2nd)
                 ,ic3rd                  =mean(ic3rd)
                 ,ic4th                  =mean(ic4th)
                 ,ic5th                  =mean(ic5th)
                 ,ic6th                  =mean(ic6th)
                 ,ic7th                  =mean(ic7th)
                 ,ic8th                  =mean(ic8th)
                 ,ic9th                  =mean(ic9th)
                 ,ic10th                 =mean(ic10th)
                 ,ic11th                 =mean(ic11th)
                 ,ic12th                 =mean(ic12th)
                 ,ic13th                 =mean(ic13th)
                 ,ic14th                 =mean(ic14th)
                 ,ic15th                 =mean(ic15th)
                 ,ic16th                 =mean(ic16th)
                 ,ic17th                 =mean(ic17th)
                 ,ic18th                 =mean(ic18th)
                 ,ic19th                 =mean(ic19th)
                 ,ic20th                 =mean(ic20th)
                 )
d10.agr <- merge(x=subset(d10.agr,authority==0),y=subset(d10.agr,authority==1,select=c("compo","m","turbulence","release","size","ticks.at80perc","effica.cum","complete.cum","compl","effica","propC","dircC","dirCoupl.50perc","inddirCoupl.50perc","ic1st","ic2nd","ic3rd","ic4th","ic5th","ic6th","ic7th","ic8th","ic9th","ic10th","ic11th","ic12th","ic13th","ic14th","ic15th","ic16th","ic17th","ic18th","ic19th","ic20th"))
                 ,by=c("compo","m","turbulence","release","size"),suffixes=c(".bu",".td"))
d10.agr$complete.cum.diff <- d10.agr$complete.cum.bu-d10.agr$complete.cum.td
d10.agr$compl.diff <- d10.agr$compl.bu-d10.agr$compl.td
d10.agr <- d10.agr[order(d10.agr$compo,d10.agr$m),]

##### Analysis: #####

### Richness:
x11(width=300,height=350)
d10.agr$authority <- d10.agr$compl.td
d10.agr$artifact  <- d10.agr$compl.bu
ggplot(subset(d10.agr,turbulence==0&release==50&m==100), aes(x=compo)) +
  #geom_vline(xintercept=.0545) +
  geom_line(aes(y=artifact*100,x=compo,colour="artifact-based coordination"),linetype="solid",size=1.25) +
  geom_line(aes(y=authority*100,x=compo,colour="authority-based coordination"),linetype="solid",size=1.25) +
  #geom_line(aes(y=compl.diff),color="red",linetype="solid",size=1) +
  theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
  theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
  ylim(c(0,1)) +
  xlab("Degree of interdependence") +
  ylab("Functionality richness (in %)") + 
  theme(axis.title=element_text(size=12,face="bold")) +
  scale_x_continuous(breaks=seq(0.005,0.250,0.010)) +
  scale_y_continuous(breaks=seq(0,100,25)) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(0.4,"in")) +
  scale_color_manual(name="Legend:", values=c("artifact-based coordination"="#00B81F", "authority-based coordination"="blue")) +
  labs(title="")
  #labs(title="(d) Functional richness with release delay under 150 available contributions")  

### Richness speed:
x11(width=300,height=350)
d10.agr$authority <- d10.agr$ticks.at80perc.td
d10.agr$artifact <- d10.agr$ticks.at80perc.bu
ggplot(subset(d10.agr,turbulence==0&release==50&m==100), aes(x=compo)) +
  #geom_vline(xintercept=.0545) +
  geom_line(aes(y=artifact,x=compo,colour="artifact-based coordination"),linetype="solid",size=1.25) +
  geom_line(aes(y=authority,x=compo,colour="authority-based coordination"),linetype="solid",size=1.25) +
  theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
  theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
  ylim(c(0,1000)) +
  xlab("Degree of interdependence") +
  ylab("Timesteps needed to reach functionality richness of 80%") + 
  theme(axis.title=element_text(size=12,face="bold")) +
  scale_x_continuous(breaks=c(seq(0.005,0.250,0.010))) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(0.4,"in")) +
  scale_color_manual(name="Legend:", values=c("artifact-based coordination"="#00B81F", "authority-based coordination"="blue")) +
  #labs(title="") 
  labs(title="(a) Timesteps needed to develop functional richness (80%) with release delay")   
  #labs(title="(b) Timesteps needed to develop functional richness (80%) \n under 150 available contributions")   

### In/Direct Coupling:
x11(width=300,height=350)
d10.agr$authority_direct <- d10.agr$dircC.td
d10.agr$artifact_direct <- d10.agr$dircC.bu
d10.agr$authority_indirect <- d10.agr$propC.td
d10.agr$artifact_indirect <- d10.agr$propC.bu
ggplot(subset(d10.agr,turbulence==0&release==50&m==100&compo<=0.080), aes(x=compo)) +
  geom_line(aes(y=artifact_direct,colour="artifact, direct",linetype="artifact, direct"),size=1.25) +
  geom_line(aes(y=authority_direct,colour="authority, direct",linetype="authority, direct"),size=1.25) +
  geom_line(aes(y=artifact_indirect,colour="artifact, indirect",linetype="artifact, indirect"),size=1.25) +
  geom_line(aes(y=authority_indirect,colour="authority, indirect",linetype="authority, indirect"),size=1.25) +
  theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
  theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
  xlab("Degree of interdependence") +
  ylab("Indirect and direct coupling") +
  ylim(0.0,1.0) +
  scale_x_continuous(breaks=c(seq(0.005,0.090,0.010),0.100)) + 
  theme(axis.title=element_text(size=12,face="bold")) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(0.4,"in")) + 
  scale_color_manual(   name="Legend:", values=c("artifact, direct"="#00B81F","authority, direct"="blue" ,"artifact, indirect"="#00B81F", "authority, indirect"="blue")) +
  scale_linetype_manual(name="Legend:", values=c("artifact, direct"="solid","authority, direct"="solid","artifact, indirect"="dotdash","authority, indirect"="dotdash")) +
  #labs(title="")
  labs(title="(a) Direct and indirect coupling with release delay")
  #labs(title="(b) Direct and indirect coupling \n under 150 available contributions")  
  # labs(title="(f) Direct and indirect coupling with release delay under 150 available contributions")  


## Relative plots:
x11(width=300,height=350)
  d10.agr.release <- subset(d10.agr,turbulence==0&m==100)
  d10.agr.release <- merge(x=subset(d10.agr.release,release==50),y=subset(d10.agr.release,release==1,select=c("compo","m","turbulence","release","size","ticks.at80perc.td","ticks.at80perc.bu","effica.cum.td","effica.cum.bu","complete.cum.td","complete.cum.bu","compl.td","compl.bu","propC.td","propC.bu","dircC.td","dircC.bu"))
                           ,by=c("compo","m","turbulence","size"),suffixes=c(".release50",".release01"))
  d10.agr.release$compl.td.diff <- d10.agr.release$compl.td.release50-d10.agr.release$compl.td.release01
  d10.agr.release$compl.bu.diff <- d10.agr.release$compl.bu.release50-d10.agr.release$compl.bu.release01
  d10.agr.release$compl.diff.diff <- d10.agr.release$compl.bu.diff-d10.agr.release$compl.td.diff
  d10.agr.release$dircC.td.diff <- d10.agr.release$dircC.td.release50-d10.agr.release$dircC.td.release01
  d10.agr.release$dircC.bu.diff <- d10.agr.release$dircC.bu.release50-d10.agr.release$dircC.bu.release01
  d10.agr.release$dircC.diff.diff <- d10.agr.release$dircC.bu.diff-d10.agr.release$dircC.td.diff
  d10.agr.release$propC.td.diff <- d10.agr.release$propC.td.release50-d10.agr.release$propC.td.release01
  d10.agr.release$propC.bu.diff <- d10.agr.release$propC.bu.release50-d10.agr.release$propC.bu.release01
  d10.agr.release$propC.diff.diff <- d10.agr.release$propC.bu.diff-d10.agr.release$propC.td.diff 
  d10.agr.release$ticks.at80perc.td.diff <- d10.agr.release$ticks.at80perc.td.release50-d10.agr.release$ticks.at80perc.td.release01
  d10.agr.release$ticks.at80perc.bu.diff <- d10.agr.release$ticks.at80perc.bu.release50-d10.agr.release$ticks.at80perc.bu.release01
  d10.agr.release <- d10.agr.release[order(d10.agr.release$compo,d10.agr.release$m),]
  
  ggplot(subset(d10.agr.release), aes(x=compo)) +
    #geom_vline(xintercept=.0545) +
    geom_line(aes(y=compl.bu.diff),color="green",linetype="solid",size=1) +
    geom_line(aes(y=compl.td.diff),color="blue",linetype="solid",size=1) +
    #geom_line(aes(y=compl.diff.diff),color="red",linetype="solid",size=1) +
    theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
    theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
    ylim(c(-1,1)) +
    xlab("Degree of interdependence") +
    ylab("Functionality richness") + 
    theme(axis.title=element_text(size=12,face="bold")) +
    scale_x_continuous(breaks=seq(0.005,0.165,0.010)) +
    labs(title="Differences in functional richness because of release delay")
  
  d10.agr.release$authority <- d10.agr.release$ticks.at80perc.td.diff
  d10.agr.release$artifact <- d10.agr.release$ticks.at80perc.bu.diff
  ggplot(subset(d10.agr.release,compo<=0.080)) +
    #geom_vline(xintercept=.0545) +
    geom_line(aes(y=artifact,x=compo,colour="artifact-based coordination"),linetype=1,size=1.25) +
    geom_line(aes(y=authority,x=compo,colour="authority-based coordination"),linetype=1,size=1.25) +
    #geom_line(aes(y=compl.diff.diff),color="red",linetype="solid",size=1) +
    theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
    theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
    ylim(c(0,1000)) +
    xlab("Degree of interdependence") +
    ylab("Functionality richness") + 
    theme(axis.title=element_text(size=12,face="bold")) +
    scale_x_continuous(breaks=seq(0.005,0.165,0.010)) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(0.4,"in")) +
    scale_color_manual(name="Legend:", values=c("artifact-based coordination"="#00B81F", "authority-based coordination"="blue")) +
    labs(title="(b) Differences in timesteps needed because of release delay")
  
  d10.agr.release$authority_direct <- d10.agr.release$dircC.td.diff
  d10.agr.release$artifact_direct <- d10.agr.release$dircC.bu.diff
  d10.agr.release$authority_indirect <- d10.agr.release$propC.td.diff
  d10.agr.release$artifact_indirect <- d10.agr.release$propC.bu.diff
  ggplot(subset(d10.agr.release,compo<=0.080), aes(x=compo)) +
    #geom_vline(xintercept=.0545) +
    geom_line(aes(y=artifact_direct,x=compo,colour="artifact, direct",linetype="artifact, direct"),size=1.25) +
    geom_line(aes(y=authority_direct,x=compo,colour="authority, direct",linetype="authority, direct"),size=1.25) +
    geom_line(aes(y=artifact_indirect,x=compo,colour="artifact, indirect",linetype="artifact, indirect"),size=1.25) +
    geom_line(aes(y=authority_indirect,x=compo,colour="authority, indirect",linetype="authority, indirect"),size=1.25) +
    theme(axis.text.x=element_text(size=rel(1.5), angle=90)) +
    theme(axis.text.y=element_text(size=rel(1.5), angle=00)) +
    ylim(c(-.6,.1)) +
    xlab("Degree of interdependence") +
    ylab("Difference in direct and indirect coupling") + 
    theme(axis.title=element_text(size=12,face="bold")) +
    scale_x_continuous(breaks=seq(0.005,0.165,0.010)) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(0.4,"in")) +
    scale_color_manual(   name="Legend:", values=c("artifact, direct"="#00B81F","authority, direct"="blue" ,"artifact, indirect"="#00B81F", "authority, indirect"="blue")) +
    scale_linetype_manual(name="Legend:", values=c("artifact, direct"="solid","authority, direct"="solid","artifact, indirect"="dotdash","authority, indirect"="dotdash")) +
    labs(title="(b) Differences in direct and indirect coupling because of release delay")
  rm(d10.agr.release)
  



  
## Coupling distributions:
x11(width=300,height=350)
barplot(as.matrix(subset(d10.agr,turbulence==0&release==1&m==100&compo==0.055,select=c("ic1st.bu","ic2nd.bu","ic3rd.bu","ic4th.bu","ic5th.bu","ic6th.bu","ic7th.bu","ic8th.bu","ic9th.bu","ic10th.bu","ic11th.bu","ic12th.bu","ic13th.bu","ic14th.bu","ic15th.bu")))
        ,names.arg=c(1:15),main=paste("(a) Artifact-based coordination and immediate release \n (interdependence=.055)",sep=""),ylim=c(0,2800),ylab="Frequency",xlab="Coupling lengths")
x11(width=300,height=350)
barplot(as.matrix(subset(d10.agr,turbulence==0&release==1&m==100&compo==0.055,select=c("ic1st.td","ic2nd.td","ic3rd.td","ic4th.td","ic5th.td","ic6th.td","ic7th.td","ic8th.td","ic9th.td","ic10th.td","ic11th.td","ic12th.td","ic13th.td","ic14th.td","ic15th.td")))
        ,names.arg=c(1:15),main=paste("(b) Authority-based coordination and immediate release \n (interdependence=.055)",sep=""),ylim=c(0,2800),ylab="Frequency",xlab="Coupling lengths")
x11(width=300,height=350)
barplot(as.matrix(subset(d10.agr,turbulence==0&release==50&m==100&compo==0.055,select=c("ic1st.bu","ic2nd.bu","ic3rd.bu","ic4th.bu","ic5th.bu","ic6th.bu","ic7th.bu","ic8th.bu","ic9th.bu","ic10th.bu","ic11th.bu","ic12th.bu","ic13th.bu","ic14th.bu","ic15th.bu")))
        ,names.arg=c(1:15),main=paste("(c) Artifact-based coordination and release delay (50 timesteps) \n (interdependence=.055)",sep=""),ylim=c(0,2800),ylab="Frequency",xlab="Coupling lengths")
x11(width=300,height=350)
barplot(as.matrix(subset(d10.agr,turbulence==0&release==50&m==100&compo==0.055,select=c("ic1st.td","ic2nd.td","ic3rd.td","ic4th.td","ic5th.td","ic6th.td","ic7th.td","ic8th.td","ic9th.td","ic10th.td","ic11th.td","ic12th.td","ic13th.td","ic14th.td","ic15th.td")))
        ,names.arg=c(1:15),main=paste("(d) Authority-based coordination and release delay (50 timesteps) \n (interdependence=.055)",sep=""),ylim=c(0,2800),ylab="Frequency",xlab="Coupling lengths")


  

z## Heatplot:
ggplot(subset(d02.agr,turbulence==0&release==1)[,c("compo","m","complete.cum.diff")], aes(compo,m)) +
  geom_tile(aes(fill=complete.cum.diff)) + 
  scale_fill_gradient2(high="green",low="blue") +
  scale_x_continuous(breaks=c(seq(0.005,0.090,0.010),0.100)) +
  labs(x="Degree of interdependence", y="Number of contributions",fill="Advantage of\nbottom-up coordination",title="Difference in functionality richness between\nbottom-up and top-down coordination")
ggplot(subset(d02.agr,turbulence==1&release==1)[,c("compo","m","complete.cum.diff")], aes(compo,m)) +
  geom_tile(aes(fill=complete.cum.diff)) + 
  scale_fill_gradient2(high="green",low="blue") +
  scale_x_continuous(breaks=c(seq(0.005,0.090,0.010),0.100)) +
  labs(x="Degree of interdependence", y="Number of contributions",fill="Advantage of\nbottom-up coordination")

