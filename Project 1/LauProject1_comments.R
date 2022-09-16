#Project 1 - Justin Lau

#load packages
suppressMessages(
  {library(ggplot2)
   library(tidyverse)
   setwd("~/Desktop/Methods/Project1/")
  }
)


#CPK: No need set the working directory when working in an R project.


#1 - dat variable containing the scales data set
dat <- read.csv("scales.csv")
 
#CPK: How can I load data that aren't in the repo? Had to add so this will run. [-2]. 

#2 - a line of code that reports the class of each column in the data set
sapply(dat,class)

#3 - a line of code that reports the dimensions of the data set
dim(dat)

#4 - Code that produces a summary of the number of scales punctured for each species
dat %>% count(species, name="# of scales punctured")

#5 - Code that produces a summary of the number of specimens sampled for each species
species <- levels(as.factor(dat$species))
dat %>% 
  count(species,specimen) %>%
  count(species,name = "# of specimens")

#6 - Code that produces a PDF file containing 6 figures, one for each species that includes a box plot of puncture force versus quadrant
pdf(file= "Lau Project1 Plots.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Well done over all. Just rember to provide all the data you need run your code.

