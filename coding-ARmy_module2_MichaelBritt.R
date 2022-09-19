#Michael Britt (coding-ARmy) Module 2 Project

#Installing Feature and Loading Libraries & Data
install.packages("features")
library(tidyverse)
library(features)
library(ggplot2)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#setwd("~/Downloads/BIOL3140 Experimental Methods in Organismal Biology/Coding-ARmy")
#not necessary for submitting this assignment

#1. Establishing the pseed.wide data tibble.
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed.wide <- pseed2%>%
  select(-amp)%>%
  pivot_wider(names_from=fin,values_from=amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  print()

#2. Computing the mean of the maximums of the amp.sums.
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x=x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$speaks)
}
pseed.max <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.sum.max <- pseed.max%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum))%>%
  print()

#3. Creating a function for the standard error of the mean (SE).
standard_error <- function(x){sd(x)/sqrt(length(x))}
pseed.sum.se <- pseed.max%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.se=standard_error(amp.sum))
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.sum.se,by=c("bl.s","fish"))

#4. Plotting the mean amp.sum vs. specific swimming speed
pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point(size=2)+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se),width=0.05,size=0.5,color="black")+geom_smooth(method="lm")+theme_classic()

#5. Merging.
pseed.met.sum <- pseed.met.rate%>%
  group_by(fish,bl.s)%>%
  summarize(met.mean=mean(met.rate),met.standard_error=standard_error(met.rate))
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.sum)%>%
  group_by(fish,bl.s)

#6. Plotting the metabolic power output of each fish vs. mean maximum of amp.sum.
pseed.sum.max%>%
  ggplot(aes(x=amp.sum.mean,y=met.mean,col=fish))+geom_point(size=2)+geom_errorbar(aes(ymin=met.mean-met.standard_error, ymax=met.mean+met.standard_error),width=0.05,size=0.5,color="black")+geom_smooth(method="lm")+theme_classic()
