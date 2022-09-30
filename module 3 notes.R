
# model = mathematical explanation of a process/system. shows relationship 
# between 2 things
# predictions in R: y~x (says that left side is predicted by right side)
  # model can be more complex ex. y~x+a, y~x+a+b+c - keep adding variables that
  # you think explains y
# linear model: lm(y~x) - to assess whether the slope is significant - is there
# a significant relationship between x/y, where x predicts y. closer to 0 = less
# of a significant relationship
# in a perfect world, would keep adding variables til you have a perfect model

# lm(y~x+a) --> says that slope are same but y int is diff - asking if the mean is diff betewen the 2 groups
# lm(y~x*a) --> says that slope is also different (use *) - asking if the response is diff between the 2 groups
  # * says that there's an interaction between x and a that affects the trend in the data

library(tidyverse)

set.seed(123) # allows you to create same random nums each time you generate a list of random nums in here
x.A=1:50
y.A=x.A*2+runif(50,1,200)
x.B=1:50
y.B=x.B*3+runif(50,1,200)
d <- tibble(x=c(x.A,x.B), y=c(y.A,y.B),species=c(rep("A",50),rep("B",50)))
d%>% ggplot(aes(x,y,col=species))+geom_point()+geom_smooth(method="lm")
  # x doesn't really predict y so make a new model based on what you see in the
  # visual representation

spec.lm1 <- lm(y~x+species,data=d)
anova(spec.lm1)
  # significant difference between means of species
summary(spec.lm1)
  # also tells you p values

spec.lm2 <- lm(y~x,d) # ,d because d is the dataset
AIC(spec.lm1,spec.lm2)
spec.lm2 <- lm(y~x*species,d)
AIC(spec.lm1,spec.lm2)
  # asks how likely it is that these data came from this model (how well do they fit the model)
  # likelihood that the model produced these data
  # but also penalize model based on how many variables it has - so looking at
  # simplest model that has a good fit to data. to minimze degrees of freedom
anova(spec.lm2)
  # x:species row says whether there's a significant interaction bw the variables
