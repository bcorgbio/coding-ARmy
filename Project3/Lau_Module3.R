# Module 3 Project - JUSTIN LAU
# Libraries ---------------------------------------------------------------
suppressMessages({
library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
library(phangorn)})

# Load Data ---------------------------------------------------------------
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

# 1. Establish the anole.log data tibble -------------------------------------
anole.log <- anole %>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

# 2. Construct two simple linear models that assess the effect of  perch height and diameter--------
anole.lm.ph <- lm(HTotal+SVL~PH, anole.log)
anole.lm.pd <- lm(HTotal+SVL~ArbPD, anole.log)

# 3. Explore how perch diameter and height affects Hindlimb-SVL relationship --------
  # Effect of Perch Height on Hindlimb-SVL Pattern
    PH2 <- seq(min(anole.log$PH),max(anole.log$PH),0.1)
    pred.lm.ph <- tibble(
      PH = PH2,
      PH.pred=predict(anole.lm.ph, newdata = data.frame(PH=PH2))
    )
    
    anole.log%>%
      ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm.ph,aes(PH,PH.pred),col="blue")

  # Effect of Perch Diameter on Hindlimb-SVL Pattern
    PD2 <- seq(min(anole.log$ArbPD),max(anole.log$ArbPD),0.1)
    pred.lm.pd <- tibble(
      ArbPD = PD2,
      PD.pred=predict(anole.lm.pd, newdata = data.frame(ArbPD=PD2))
    )
    
    anole.log%>%
      ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm.pd,aes(ArbPD,PD.pred),col="blue")

# 4. Use a BM model of trait evolution and construct phylogenetic least squares models --------
  anole.tree <- read.tree("anole.tre")

  # A PGLS model with the hindlimb-SVL relationship + perch height
    pgls.BM.ph <- gls(HTotal~SVL + PH, 
                      correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                      data= anole.log, method = 'ML')

  # A PGLS model with the hindlimb-SVL relationship + perch diameter
    pgls.BM.pd <- gls(HTotal~SVL + ArbPD, 
                      correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                      data= anole.log, method = 'ML')
    
  # A PGSL model with the hindlimb-SVL relationship + perch height + perch diameter
    pgls.BM.ph.pd <- gls(HTotal~SVL + PH + ArbPD, 
                         correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                         data= anole.log, method = 'ML')
    
# 5.  Assess the fit of each of the three models using AICc and AICw --------
    anole.phylo.aic <- AICc(pgls.BM.ph,pgls.BM.pd,pgls.BM.ph.pd)
    aicw(anole.phylo.aic$AICc)

# 6. Produce a plot of your own design to concisely visualize effe --------







