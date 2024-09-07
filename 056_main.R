install.packages("plyr",dependencies=T)
install.packages("Hmisc",dependencies=T)
install.packages("rgl",dependencies=T)
install.packages("plot3D",dependencies=T)
install.packages("expm",dependencies=T)
install.packages("dplyr",dependencies=T)
install.packages("beepr",dependencies=T)
install.packages("Rsolnp",dependencies=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/qualityTools/qualityTools_1.55.tar.gz",dependencies=T,repos=NULL)
install.packages("ggplot2",dependencies=T)

library(plyr)
library(Hmisc)
library(rgl)
library(plot3D)
library(expm)
library(dplyr)
library(beepr)
library(Rsolnp)
library(qualityTools) 
library(ggplot2)
library(remotes)

install_github("Zaggl/coordinationSimulationModel")
library(coordinationSimulationModel)

Sys.setenv(LANG="en")
options(max.print=9999999)

##### Full experiments: #####

## Running the simulation experiments takes roughly 6.5 hours (see requirements.txt for hardware specifications);
## uncomment the following line for using from previous simulation runs instead of running the simulation experiments:
d10 <- read.csv("data/data_d10.csv",sep=",")

## Single run (with graph) over time:
getExperiment(authority.v=c(T,F),ticks=1000,composability.v=0.070,m.v=c(100),f.v=c(1.00),turbulence.v=c(0),release.v=c(1),size=100,repetitions=1,seed=28071984,plot=T) 

d10.01 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.085,0.005)),m.v=c(100)                         ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=28071984,plot=F) 
d10.02 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.085,0.005)),m.v=c(100)                         ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=13012010,plot=F) 
d10.03 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.085,0.005)),m.v=c(100)                         ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=16012014,plot=F)
d10.04 <- getExperiment(authority.v=c(T,F),ticks=1000,composability.v=c(seq(0.005,0.085,0.005)),m.v=c(100)                         ,f.v=c(1.00),turbulence.v=c(0),release.v=c(1,50),size=100,repetitions=025,seed=07032018,plot=F) 
d10 <- rbind(d10.01,d10.02,d10.03,d10.04)

write.table(d10,"data/data_d10.csv",sep=",")

##### Aggregate data from experiments (to average repetitions): #####

## Uncomment the following line for using the aggregated data from previous simulation runs:
d10.agr <- read.csv("data/data_d10.agr.csv",sep=",")

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
                 ,ic1st                  =mean(ic1st,na.rm=T)
                 ,ic2nd                  =mean(ic2nd,na.rm=T)
                 ,ic3rd                  =mean(ic3rd,na.rm=T)
                 ,ic4th                  =mean(ic4th,na.rm=T)
                 ,ic5th                  =mean(ic5th,na.rm=T)
                 ,ic6th                  =mean(ic6th,na.rm=T)
                 ,ic7th                  =mean(ic7th,na.rm=T)
                 ,ic8th                  =mean(ic8th,na.rm=T)
                 ,ic9th                  =mean(ic9th,na.rm=T)
                 ,ic10th                 =mean(ic10th,na.rm=T)
                 ,ic11th                 =mean(ic11th,na.rm=T)
                 ,ic12th                 =mean(ic12th,na.rm=T)
                 ,ic13th                 =mean(ic13th,na.rm=T)
                 ,ic14th                 =mean(ic14th,na.rm=T)
                 ,ic15th                 =mean(ic15th,na.rm=T)
                 ,ic16th                 =mean(ic16th,na.rm=T)
                 ,ic17th                 =mean(ic17th,na.rm=T)
                 ,ic18th                 =mean(ic18th,na.rm=T)
                 ,ic19th                 =mean(ic19th,na.rm=T)
                 ,ic20th                 =mean(ic20th,na.rm=T)
)
d10.agr <- merge(x=subset(d10.agr,authority==0),y=subset(d10.agr,authority==1,select=c("compo","m","turbulence","release","size","ticks.at80perc","effica.cum","complete.cum","compl","effica","propC","dircC","dirCoupl.50perc","inddirCoupl.50perc","ic1st","ic2nd","ic3rd","ic4th","ic5th","ic6th","ic7th","ic8th","ic9th","ic10th","ic11th","ic12th","ic13th","ic14th","ic15th","ic16th","ic17th","ic18th","ic19th","ic20th"))
                 ,by=c("compo","m","turbulence","release","size"),suffixes=c(".bu",".td"))
d10.agr$complete.cum.diff <- d10.agr$complete.cum.bu-d10.agr$complete.cum.td
d10.agr$compl.diff <- d10.agr$compl.bu-d10.agr$compl.td
d10.agr <- d10.agr[order(d10.agr$compo,d10.agr$m),]

write.table(d10.agr,"../data/data_d10.agr.csv",sep=",")


##### Analysis: #####

#### Figure 5 in Research Paper: #####
### Propagation cost: 
x11(width=300,height=350)
d10.agr$authority_direct <- d10.agr$dircC.td
d10.agr$artifact_direct <- d10.agr$dircC.bu
d10.agr$authority_indirect <- d10.agr$propC.td
d10.agr$artifact_indirect <- d10.agr$propC.bu
ggplot(subset(d10.agr,turbulence==0&release==01&m==100&compo<=0.080), aes(x=compo)) +
  geom_line(aes(y=artifact_indirect ,colour="artifact-based" ,linetype="artifact-based" ),size=1.5) +
  geom_line(aes(y=authority_indirect,colour="authority-based",linetype="authority-based"),size=1.5) +
  geom_point(aes(y=artifact_indirect ,colour="artifact-based" ,shape="artifact-based") ,size=7) +
  geom_point(aes(y=authority_indirect,colour="authority-based",shape="authority-based"),size=7) +
  theme(axis.text.x=element_text(size=rel(2.5), angle=90)) +
  theme(axis.text.y=element_text(size=rel(2.5), angle=00)) +
  xlab("Degree of task interdependence") +
  ylab("Propagation costs") +
  ylim(0.0,1.0) +
  scale_x_continuous(breaks=c(seq(0.005,0.090,0.010),0.100)) + 
  theme(axis.title=element_text(size=26,face="bold")) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=26)) +
  #theme(legend.key.size=unit(0.4,"in")) + 
  scale_color_manual(   name="", values=c("artifact-based"="#00B81F","authority-based"="blue")) +
  scale_linetype_manual(name="", values=c("artifact-based"="solid"  ,"authority-based"="solid")) +
  scale_shape_manual(   name="", values=c("artifact-based"=16       ,"authority-based"=17)) + 
  labs(title="") + 
  ##labs(title="Baseline model (used in the mansucript)") + 
  theme(plot.title=element_text(size=26))

#### Figure 6a in Research Paper: #####
### Propagation cost with release delay:
x11(width=300,height=350)
d10.agr.release <- subset(d10.agr,turbulence==0&m==100)
d10.agr.release <- merge(x=subset(d10.agr.release,release==50),y=subset(d10.agr.release,release==01,select=c("compo","m","turbulence","release","size","ticks.at80perc.td","ticks.at80perc.bu","effica.cum.td","effica.cum.bu","complete.cum.td","complete.cum.bu","compl.td","compl.bu","propC.td","propC.bu","dircC.td","dircC.bu"))
                         ,by=c("compo","m","turbulence","size"),suffixes=c(".release50",".release01"))
d10.agr.release$artifact_immediate  <- d10.agr.release$propC.bu.release01
d10.agr.release$artifact_delayed    <- d10.agr.release$propC.bu.release50
d10.agr.release$authority_immediate <- d10.agr.release$propC.td.release01
d10.agr.release$authority_delayed   <- d10.agr.release$propC.td.release50
ggplot(subset(d10.agr.release,compo<=0.08,), aes(x=compo)) +
  geom_line(aes(y=artifact_immediate  ,colour="artifact, full" ,linetype="artifact, full") ,size=1.5) +
  geom_line(aes(y=artifact_delayed    ,colour="artifact, delayed"   ,linetype="artifact, delayed")   ,size=1.5) +
  geom_line(aes(y=authority_immediate ,colour="authority, full",linetype="authority, full"),size=1.5) +
  geom_line(aes(y=authority_delayed   ,colour="authority, delayed"  ,linetype="authority, delayed")  ,size=1.5) +
  geom_point(aes(y=artifact_immediate ,colour="artifact, full" ,shape="artifact, full") ,size=7) +
  geom_point(aes(y=artifact_delayed   ,colour="artifact, delayed"   ,shape="artifact, delayed")   ,size=7) +
  geom_point(aes(y=authority_immediate,colour="authority, full",shape="authority, full"),size=7) +
  geom_point(aes(y=authority_delayed  ,colour="authority, delayed"  ,shape="authority, delayed")  ,size=7) +
  theme(axis.text.x=element_text(size=rel(2.5), angle=90)) +
  theme(axis.text.y=element_text(size=rel(2.5), angle=00)) +
  xlab("Degree of task interdependence") +
  ylab("Propagation costs") +
  ylim(0.0,1.0) +
  scale_x_continuous(breaks=c(seq(0.005,0.090,0.010),0.100)) + 
  theme(axis.title=element_text(size=26,face="bold")) +
  theme(legend.position="") + ## right
  theme(legend.text=element_text(size=26)) + ## 14
  theme(legend.key.width=unit(1.5,"cm")) +
  theme(legend.key.size=unit(0.4,"in")) + 
  scale_color_manual(   name="", values=c("authority, full"="blue"  ,"authority, delayed"="blue" , "artifact, full"="#00B81F","artifact, delayed"="#00B81F")) +
  scale_linetype_manual(name="", values=c("authority, full"="solid","authority, delayed"="dashed", "artifact, full"="solid" ,"artifact, delayed"="dashed"  )) +
  scale_shape_manual   (name="", values=c("authority, full"=17      ,"authority, delayed"=02     , "artifact, full"=16       ,"artifact, delayed"=01       )) + 
  #labs(title="") +
  labs(title="(a) Visibility delay (each 50th\n timestep) vs full visibility") +
  theme(plot.title=element_text(size=26,face="bold"))
  rm(d10.agr.release)

#### Figure 6b in Research Paper: #####
## Difference plots:
x11(width=300,height=350)
  d10.agr.release <- subset(d10.agr,turbulence==0&m==100)
  d10.agr.release <- merge(x=subset(d10.agr.release,release==50),y=subset(d10.agr.release,release==01,select=c("compo","m","turbulence","release","size","ticks.at80perc.td","ticks.at80perc.bu","effica.cum.td","effica.cum.bu","complete.cum.td","complete.cum.bu","compl.td","compl.bu","propC.td","propC.bu","dircC.td","dircC.bu"))
                           ,by=c("compo","m","turbulence","size"),suffixes=c(".release50",".release01"))
  d10.agr.release$compl.td.diff <- d10.agr.release$compl.td.release50-d10.agr.release$compl.td.release01
  d10.agr.release$compl.bu.diff <- d10.agr.release$compl.bu.release50-d10.agr.release$compl.bu.release01
  d10.agr.release$compl.diff.diff <- d10.agr.release$compl.bu.diff-d10.agr.release$compl.td.diff
  d10.agr.release$dircC.td.diff <- d10.agr.release$dircC.td.release50-d10.agr.release$dircC.td.release01
  d10.agr.release$dircC.bu.diff <- d10.agr.release$dircC.bu.release50-d10.agr.release$dircC.bu.release01
  d10.agr.release$dircC.diff.diff <- d10.agr.release$dircC.bu.diff-d10.agr.release$dircC.td.diff
  d10.agr.release$propC.td.diff <- d10.agr.release$propC.td.release50-d10.agr.release$propC.td.release01
  d10.agr.release$propC.bu.diff <- d10.agr.release$propC.bu.release50-d10.agr.release$propC.bu.release01
  d10.agr.release$propC.td.diff.rel <- (d10.agr.release$propC.td.release50-d10.agr.release$propC.td.release01)/d10.agr.release$propC.td.release01
  d10.agr.release$propC.bu.diff.rel <- (d10.agr.release$propC.bu.release50-d10.agr.release$propC.bu.release01)/d10.agr.release$propC.bu.release01
  d10.agr.release$propC.diff.diff <- d10.agr.release$propC.bu.diff-d10.agr.release$propC.td.diff 
  d10.agr.release$ticks.at80perc.td.diff <- d10.agr.release$ticks.at80perc.td.release50-d10.agr.release$ticks.at80perc.td.release01
  d10.agr.release$ticks.at80perc.bu.diff <- d10.agr.release$ticks.at80perc.bu.release50-d10.agr.release$ticks.at80perc.bu.release01
  d10.agr.release <- d10.agr.release[order(d10.agr.release$compo,d10.agr.release$m),]  
  d10.agr.release$authority_direct <- d10.agr.release$dircC.td.diff
  d10.agr.release$artifact_direct <- d10.agr.release$dircC.bu.diff
  d10.agr.release$authority_indirect <- d10.agr.release$propC.td.diff.rel
  d10.agr.release$artifact_indirect <- d10.agr.release$propC.bu.diff.rel
  ggplot(subset(d10.agr.release,compo<=0.080), aes(x=compo)) +
    #geom_vline(xintercept=.0545) +
    geom_line(aes(y=artifact_indirect ,x=compo,colour="artifact-based, relative difference" ,linetype="artifact-based, relative difference"),size=1.5) +
    geom_line(aes(y=authority_indirect,x=compo,colour="authority-based, relative difference",linetype="authority-based, relative difference"),size=1.5) +
    geom_point(aes(y=artifact_indirect ,x=compo,colour="artifact-based, relative difference" ,shape="artifact-based, relative difference"),size=7) +
    geom_point(aes(y=authority_indirect,x=compo,colour="authority-based, relative difference",shape="authority-based, relative difference"),size=7) +
    theme(axis.text.x=element_text(size=rel(2.5), angle=90)) +
    theme(axis.text.y=element_text(size=rel(2.5), angle=00)) +
    ylim(c(-1.0,0.1)) +
    xlab("Degree of task interdependence") +
    ylab("Difference in propagation costs (in %)") + 
    theme(axis.title=element_text(size=26,face="bold")) +
    scale_x_continuous(breaks=seq(0.005,0.165,0.010)) +
    theme(legend.position="none") + # right
    theme(legend.text=element_text(size=26)) +
    theme(legend.key.width=unit(1.5,"cm")) +
    theme(legend.key.size=unit(0.4,"in")) +
    scale_color_manual(   name="", values=c("artifact-based, relative difference"="#00B81F","authority-based, relative difference"="blue")) +
    scale_linetype_manual(name="", values=c("artifact-based, relative difference"="dotted" ,"authority-based, relative difference"="dotted")) +
    scale_shape_manual(   name="", values=c("artifact-based, relative difference"=13       ,"authority-based, relative difference"=11)) + 
    labs(title="(b) Percentage differences \n in propagation costs") + 
    theme(plot.title=element_text(size=26,face="bold"))
  rm(d10.agr.release)
  
