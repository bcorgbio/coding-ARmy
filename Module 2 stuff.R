
install.packages("features")

library(tidyverse) # Remember to load your libraries!
library(features)

#don't have to set working directory in an R project because connected to repository

#load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
  # using read_csv instead of read.csv because loads data as a tibble, an 
  # object like a data.frame but one that works seamlessly with the other 
  # functions contained in tidyverse
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
  # The left_join() operation takes the speeds table and joins it leftward to 
  # the pseed table
  # The by parameter specifies by what columns to join the tables, i.e., the 
  # key values for each. Notice that the names of the key values are different 
  # because the key columns have different names in pseed and speeds. With 
  # print(), we printed the new table, complete with m.s and cm.s columns. 
  # Notice that there’s no vol column added from the speeds table

pseed.bl%>%
  print()
pseed2%>%
  select(fish)%>%
  unique()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
  # redefining pseed2
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
  # added new column
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
  # changing transparency of plot
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()
  # just looking at the fins in one of the experiments

exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
  # making frame the x value and amp.bl the y value
fget(f1)
  # gets important stuff from the results
  # shows that there's 24 critical points where the curve is 0 (max/mins)
    # neg values are peaks and pos are troughs. it's showing zero because the
    # values are really small and it rounds, so it rounds to zero
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
  # plotting data w lines thru the critical points
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
  # multiplying amps by 100 so that our curvature values don't round to zero and
  # we can see peaks vs troughs w neg/pos curvature values
fget(f2)

f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()
  # took the critical points and curvatures of the f2 output from features() 
  # and passed it to a new tibble
  # filter is choosing only the neg values of curvature so we only get the peaks
  # the frames were being expressed as decimals (in crit.points) so the mutate 
  # rounds them to integers (bc obvi frames r integers)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()
  # shows peaks for one experiment
  
pseed2%>%
  summarize(n=length(unique(date)))
  # shows that we have 50 experiments total! how to get the peaks for all of them??

# creating a custom func:
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we want to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd items (the critical points and curvature)
    as_tibble()%>% # putting data in a tibble
    filter(curvature<0)%>% # filter that returns curvatures <0
    mutate(peaks=round(crit.pts,0)) #adds a column that rounds the critical point to an integer that represents the frame
  return(f$peaks) # return the peaks from tibble
}

pseed2%>%
  filter(date%in%unique(date)[1:3])%>% # only looking at first 3 experiments in pseed2
  group_by(date,fin)%>% # grouped by date (experiment) and fin
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>% # adds a peak column to data set - based on integer we got for frames
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)
    # plotted amp over frame, color diff dependent on whether peak or not
    # facet_grid(date~fin) makes diff grids based on the diff dates/fins

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter
  # looking at all the data so just taking out the parts that are peaks to simplify it

pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")
  # plotting amplitude against swimming speed
  # looks like amp decreases w speed

amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)
  # running anova to see if above data are statistically significant (if amp
  # really decreases as speed increases). P value is lower than .5 so it is
  # a significant relationship

pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")
  # plotting mean vs speed for each fish

pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
  # adding max amp of left and right fin for each fish in each experiment at 
  # each frame to give amp sum. however, left and right fins are still in 2
  # diff rows so the amp.sum column has duplicate values (one in each row for
  # left or right fin)
pseed2 %>%
  filter(fin=="R")
  # filters data so that we only have rows for right fins. but this makes it
  # look like the amp.sum only applies to the right fin, and we've lost half
  # the data! so we should instead make the data wider so that left and right
  # fin can be in diff columns and each have their own amp

pseed.wide <- pseed2 %>%
  select(-amp)%>% # removed amp column so we just have amp.bl
  pivot_wider(names_from = fin,values_from = amp.bl) %>% # adds 2 new columns. names of columns come from fin column, values for these columns come from amp.bl column
  mutate(amp.sum=L+R)%>% # adding new amp.sum column
  print() 
  





