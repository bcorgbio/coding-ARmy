#Katherine Quinlivan Module 2 Project

library(tidyverse)
library(ggplot2)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#1 establish pseed.wide

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#CPK: ^^ all these operations cans be piped together, a la

pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 



#2 compute mean and maximums of all amp.sums

find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.sum.max <- pseed.max%>%
  group_by(fish, speed)%>%
  summarize(amp.sum.mean=(amp.sum))
  #print()

#3 custom function to compute standard error of the mean (standErr)
  
standErr <- function(x) {
  as.numeric(x)
  sd(x)/ sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = standErr(amp.sum)) 
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed", "fish")) 


#CPK: It would have been more concise to define your se function early and calculated se with mean. [-1] Like  . . .

pseed.sum.max <- pseed.max %>%
  group_by(fish,speed)%>% # grouped by fish and speed
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se=standErr(amp.sum))

#4 plot of amp.sum.mean vs swimming speed

pseed.sum.max %>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish)) + geom_point()+ geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.01, colour="black") + theme_classic()

#CPK: ^^ Didn't we want our error bars to be color coded as well? [-1]

#5 merge with with pseed.sum.max

pseed.max <- pseed.max%>%
  merge(pseed.met.rate,by=c("fish","date","m.s","cm.s","bl.s"))
pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.met.rate=mean(met.rate))
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate)%>%
  group_by(fish,speed)

#6 plot metabolic power output of each fish vs. mean maximum of amp.sum

pseed.sum.max %>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish)) + geom_point() + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se ,ymax= amp.sum.mean+amp.sum.se), width=0.001, colour="black") + theme_classic()

#CPK: Nice work adding the error bars (be sure they're colored, too!). Didn't ask for this, but the information is there. 


#CPK: Really well done! Great work!