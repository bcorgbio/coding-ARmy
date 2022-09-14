
# Install Packages --------------------------------------------------------
install.packages("features")

# Libraries ---------------------------------------------------------------
suppressMessages(
  {library(ggplot2)
    library(tidyverse)
    library(features)
    setwd("~/Desktop/Methods/Project2/")
  })

# Data --------------------------------------------------------------------
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


# Mutate Data -------------------------------------------------------------
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#gives amp.sum
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

# 2. Compute mean maximum* of all the amp.sums ----------------------------
pseed.wide%>%
ggplot(aes(x=speed,y=amp.sum,col=fish))+geom_point()

find.max <- function (x,y, mult=100){
  fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}
  

pseed.sum.max <- pseed.wide %>% 
  find.max(amp.sum,speed)
  



pseed.sum.max <- (mean(max(pseed2$amp.sum)))
