#Katherine Quinlivan Project 1


library(ggplot2)
library(tidyverse)

setwd("~/Documents/biol3140/coding-ARmy/scales")

dat <- read.csv("scales.csv")

sapply(dat,class)

dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
length(species)

species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")


for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

list.files(pattern=".pdf")
