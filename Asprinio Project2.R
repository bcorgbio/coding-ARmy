
library(tidyverse)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
  # download data sets

#1 establish pseed.wide data tibble

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>% 
    # left_join() operation takes the speeds table and joins it leftward to the
    # pseed table. creates a new dataframe of this combo. now we have m/s and cm/s columns
  left_join(pseed.bl,by="fish")%>%
    # joining pseed.bl to new tibble. now we have a new bl (in cm) column
  mutate(bl.s=cm.s/bl)%>%
    # add a new column with bl/s by dividing cm/s by bl (in cm)
  print() 
  # in module, broke this into multiple steps. combined them with pipe

pseed.wide <- pseed2 %>%
  select(-amp)%>% # removed amp column so we just have amp.bl
  pivot_wider(names_from = fin,values_from = amp.bl) %>% # adds 2 new columns. names of columns come from fin column, values for these columns come from amp.bl column. so fin and amp.bl columns are now gone
  mutate(amp.sum=L+R)%>% # adding new amp.sum column
  print() 

#2 mean of max amp sums for each fish at each speed

# creating a custom func:
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we want to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd items (the critical points and curvature)
    as_tibble()%>% # putting data in a tibble
    filter(curvature<0)%>% # filter that returns curvatures <0
    mutate(peaks=round(crit.pts,0)) #adds a column that rounds the critical point to an integer that represents the frame
  return(f$peaks) # return the peaks from tibble
}
pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>% # using the func we made
  filter(peak==T) #new filter
  # looking at all the data so just taking out the parts that are peaks to simplify it

pseed.sum.max <- pseed.max %>%
  group_by(fish,speed)%>% # grouped by fish and speed
  summarize(amp.sum.mean=mean(amp.sum)) %>%
  print()

#3 custom function that computes the standard error of the mean (SE)
#  new column in summary table pseed.sum.max for SE called amp.sum.se

calc.se <- function(x) {
  as.numeric(x)
  standard_dev <- sd(x)
  standard_error <- standard_dev/sqrt(length(x))
  return(standard_error)
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = calc.se(amp.sum)) # new column based on standard errors of amp sums vs means
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed","fish")) # adding column from other table to here

#4 plot of amp.sum.mean vs speed with error bars

pseed.sum.max %>%ggplot(aes(x=speed, y=amp.sum.mean, col = fish)) + geom_point(size=2) + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width = 0.05, size = 0.5)+geom_smooth(method="lm")+theme_classic()

#5 merge new data with pseed.sum.max

pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.max <- pseed.max%>%
  merge(pseed.met.rate,by=c("fish","date","m.s","cm.s","bl.s"))
pseed.mean.rate <- pseed.max %>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate=mean(met.rate))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.mean.rate, by = c("speed","fish"))

#6 plot of metabolic power output of each fish vs. mean maximum of amp.sum

pseed.sum.max %>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.05, colour="black")+theme_classic()
