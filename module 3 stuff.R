
install.packages("ape")
install.packages("nlme")
install.packages("geiger")
install.packages("caper")
install.packages("phytools")
install.packages("viridis")
install.packages("MuMIn")
install.packages("phangorn")
install.packages("cowplot")

library(tidyverse) # Remember to load your libraries!
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
library(phangorn)
library(cowplot)

#load data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole%>%
  left_join(anole.eco)%>% # merged data sets to create a new one
  filter(!Ecomorph%in%c("U","CH"))%>% # removed rows containing U or CH
  na.omit()%>% # omitting all rows that say n/a
  print()
  
anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)
  # changed data to log form to bring data closer to a normal distribution and
  # to make everything a nice ration w the other stuff

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")
  # seeing if hind limb length varies w size
  # lm (linear model) is a good place to start for 2 continuous variables
anole.lm <- lm(HTotal~SVL,anole2)
coef(anole.lm)
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")
  # looking at same data with a simple linear model

SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
)
  # creating a tibble w predictions to predict HVTotal instead of geom_smooth()
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")
summary(anole.lm)
  # p values of y int and slope are below .05 so linear graph of data seems to 
  # fit pretty well. but other stuff may fit better

anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)
summary(anole.allo)
  # fitting an allometric model tot he data to see if it fits better
  # put in starting parameters of what you think may fit the data best

#AICc from the MuMIn package
anole.aic <- AICc(anole.lm,anole.allo)

#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

print(anole.aicw)
  # shows that the second model, anole.allo has a lower AIC score, and lower 
  # scores indicate a better fit
  # a ΔAIC of less the 4 indicates roughly equivalent models, 4–8 little 
  # support for a lesser fit, and over 10 a poor fit and the model can be 
  # ignored
  # because this gives ΔAIC<4, there’s little difference between our linear and
  # allometric models of hind-limb growth; thus, we can say that allometry and 
  # isometry are roughly equivalent models. to choose better one, use likelihood:

logLik(anole.lm)
logLik(anole.allo)
  # shows that allometric data has a higher likelihood of being right so this
  # is the model we'll use

anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")
  # col=Ecomorph2 colors all the added geometries according to the column values 
  # in Ecomorph2, a longer description of ecomorph than the abbreviations one 
  # Ecomorph. In addition, this establishes groups of data to compute multiple 
  # regression lines for `geom_smooth()
  # shows that we should consider ecomorph as an important variable explaining 
  # the hindlimb-SVL relationship

anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
  # constructing a model that includes ecomorph as a variable. includes interaction
summary(anole.log.eco.lm)
anova(anole.log.eco.lm)
  # anova is a two-way analysis of covariance. That is, we are assessing the 
  # effect of a categorical variable (Ecomorph2) in the context of how HTotal 
  # covaries with SVL. The results of the ANOVA indicate that we should reject
  # the null hypothesis that ecomorph groups do not have separate hindlimb-SVL
  # relationships (so they do have separate relationships). This is indicated 
  # by the p=3.761e-09 for the Ecomorph2 variable
  # we have established that ecomorph has a significant effect on the 
  # hindlimb-SVL relationship
  # but still wanna check if the model including this variable is better than
  # a simpler one (without this variable)

anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)
anole.log.aic <- AICc(anole.log.lm,anole.log.eco.lm)
aicw(anole.log.aic$AICc)
  # show that a model with the added Ecomorph2 parameter results in a much 
  # better fit, both in terms of ΔAIC and AICw.

anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))
  # computing residuals for the anole.log.lm model
  # we used a mutate() operation to establish a new res column that contains the residuals
  # redefined anole.log as a tibble containing this new column
anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()
  # plotting residuals against Ecomorph2
p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
  # to visualize median of the residuals
print(p.eco)
p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
  # to visualize mean of the residuals
  # added a point with the stat_summary() geometry. This applies a summarizing
  # function to the already established group (established by x=Ecomorph2) and
  # plots the value of the summary with a specified geometry. The function we 
  # applied to the y data is the mean (fun=mean) and we specified a point with
  # size 3 for the geometry to add (geom=point and size=3, respectively)


# have to include phylogeny!!!! it may be that any patterns we’ve uncovered 
# thus far are the result of phylogeny, not merely ecomorph alone

anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)
  # plot of evolutionary tree
  # read in the tree using read.tree from the ape package. This stores the 
  # tree as a phylo object, a list of branch order, branch lengths, and tip 
  # labels. We’ll use the phylo object in our analysis. One can plot phylo 
  # objects with the plot() function. We added cex=0.4. cex stands for the 
  # character expansion factor—-how the size of the text should be adjusted. 
  # In this case, the tip label (i.e., species names) are too cluttered if we 
  # don’t reduce the character size to 0.4 of the default.

#PGLS under BM, no ecomorph
pgls.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, w ecomorph
pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")


#PGLS under OU, no ecomorph
pgls.OU1 <- gls(HTotal ~SVL, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under OU, w, ecomorph
pgls.OU2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
  # set up generalized least squares models that establish a correlation 
  # matrix based on BM and OU models of evolution. We establish this matrix 
  # with corBrownian() from the ape package for the BM models and corMartins(), 
  # also from ape, for the OU models. we don’t have to specify what any of the
  # parameters are; they are estimated under maximum likelihood when method=ML. 
  # For the BM model, we used a stating starting value of 1 for σ2 and for the 
  # OU model we used a starting value of 0 for α. In each correlation function,
  # we have to specify the tree (anole.tree) and the column names in data that 
  # will form the covariation matrix, that is, species (~Species)


anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.OU1,pgls.OU2)
aicw(anole.phylo.aic$AICc)
  # to see which model fits data best - it's the one w ecomorph

anova(pgls.BM2)
  # performing anova on the best-fitting model to see whether ecomorph is a
  # significant factor





