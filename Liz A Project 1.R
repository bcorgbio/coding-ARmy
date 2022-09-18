
library(ggplot2)
library(tidyverse)
  # load packages

setwd("~/Desktop/ecuador stuff")
  # set directory

dat <- read.csv("scales.csv")
  #1 - variable with dataset

sapply(dat,class)
  #2 - class of each column in dataset

dim(dat)
  #3 - dimensions of dataset

dat %>% count(species, name="n.scales punctured")
  #4 - summary of number of scales punctured for each species

species <- levels(as.factor(dat$species))
  # define species column as factor, break it into diff levels (in this case, diff species)
dat %>% 
  count(species,specimen) %>%
  count(species,name = "n.specimens")
  #5 - summary of number of specimens sampled for each species

for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
  # plots each species
pdf("Liz A Project1 Boxplots.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")
  #6 - produces pdf file with boxplots




