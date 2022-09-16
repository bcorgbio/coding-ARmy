# Module 2 Project - JUSTIN
# Install Packages --------------------------------------------------------
install.packages("features")

# Libraries ---------------------------------------------------------------
suppressMessages(
  {library(ggplot2)
    library(tidyverse)
    library(features)
    library(features)
    setwd("~/Desktop/Methods/Project2/")
  })

# Data --------------------------------------------------------------------
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


# 1. Establish pseed.wide--------------------------------------------------
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

# 2. Compute mean maximum* of all the amp.sums ----------------------------
pseed.max <- pseed.wide %>%
  group_by(fish, speed) %>%
  summarize(max=max(amp.sum))

pseed.sum.max <- pseed.max %>%
  group_by(fish)%>%
  summarize(amp.sum.mean = mean(max))

# 3.  Custom function to compute standard error of the mean (SE) --------
SE <- function(x){
  sd(x)/ sqrt(length(x))
}
pseed.sum.se <- pseed.max %>%
  group_by(fish)%>%
  summarize(amp.sum.se = SE(max))
pseed.sum.max <- pseed.sum.max %>%
  inner_join(pseed.sum.se, by = "fish")

# 4. Plot mean amp.sum vs specific swimming speed -------------------------
pseed.max %>%
  group_by(fish, speed) %>%
  ggplot(aes(x=speed,y=max,col=fish))+geom_point()+geom_smooth(method="lm")


# 5.  merge with pseed.sum.max --------------------------------------------
pseed.met.rate <- read_csv("pseed.met.rate.csv")


