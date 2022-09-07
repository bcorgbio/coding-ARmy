#Project 1 

suppressMessages(
  {library(ggplot2)
    library(tidyverse)
    setwd("~/Desktop/Methods/")
  }
  )

#1 - dat variable containing the scales dataset
dat <- read.csv("scales.csv")

#2 - line of code that reports the class of each column in the data set
for(i in 1:4){
  cat(colnames(dat)[i],":", class(dat[,i]),"\n")
}

#3 - line of code that reports the dimensions of the data set
dim(dat)

#4 - Code that produces a summary of the number of scales punctured for each species
dat %>% count(species)

#5 - Code that produces a summary of the number of specimens sampled for each species
species <- levels(as.factor(dat$species))
dat %>% 
 count(species,specimen) %>%
 count(species,name = "n.specimens")


#6 - Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force verus quadrant

pdf(file= "Lau Project1 Plot.pdf")
for(i in species){
  with(dat, print(ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)))
}
dev.off() 
