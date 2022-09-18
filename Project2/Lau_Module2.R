# Module 2 Project - JUSTIN

# Install Packages --------------------------------------------------------
install.packages("features")

# Libraries ---------------------------------------------------------------
suppressMessages(
  {library(ggplot2)
    library(tidyverse)
    library(features))
  })

# Data --------------------------------------------------------------------
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

# 1. Establish pseed.wide--------------------------------------------------
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)
# 2. Compute mean maximum* of all the amp.sums ----------------------------
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.max$peak <- NULL

pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum)) 
  
# 3.  Custom function to compute standard error of the mean (SE) --------
SE <- function(x){
  sd(x)/ sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = SE(amp.sum))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed","fish"))

# 4. Plot mean amp.sum vs specific swimming speed -------------------------
pd <- position_dodge(0.1)
pseed.sum.max %>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.5, colour="black", position=pd)+theme_classic()
# 5. Merge met rate with pseed.sum.max --------------------------------------------
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.max <- pseed.max%>%
  merge(pseed.met.rate,by=c("fish","date","m.s","cm.s","bl.s"))
pseed.mean.rate <- pseed.max %>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate=mean(met.rate))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.mean.rate, by = c("speed","fish"))

# 6. plot  metabolic power output vs. amp.sum.mean --------
pseed.sum.max %>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.05, colour="black", position=pd)+theme_classic()

