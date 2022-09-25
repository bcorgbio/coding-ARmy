# Module 3 Project - Coding aRmy

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
  #Simple Log Linear Model
  anole.log.lm <- lm(HTotal~SVL, anole.log)

  # Perch Height
  anole.log.ph.lm <- lm(HTotal~SVL+PH, anole.log)
  
  # Perch Diameter
  anole.log.pd.lm <- lm(HTotal~SVL+ArbPD, anole.log)

# 3. Explore how perch diameter and height affects Hindlimb-SVL relationship --------
  # Add Residuals
  anole.log <- anole.log %>%
    mutate(res.ph = residuals(anole.log.ph.lm), 
           res.pd = residuals(anole.log.pd.lm))
  
  # Effect of Perch Height on Hindlimb-SVL Pattern
  anole.log%>%
    ggplot(aes(x=PH,y=res.ph)) +geom_boxplot() 
# Effect of Perch Diameter on Hindlimb-SVL Pattern
  anole.log%>%
    ggplot(aes(x=ArbPD,y=res.pd)) +geom_boxplot() 

# 4. Use a BM model of trait evolution and construct phylogenetic least squares models --------
  anole.tree <- read.tree("anole.tre")

  pgls.BM <- gls(HTotal~SVL, 
                 correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                 data= anole.log, method = 'ML')
  
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
#PH
anole.phylo.aic <- AICc(pgls.BM, pgls.BM.ph, pgls.BM.pd, pgls.BM.ph.pd)
aicw(anole.phylo.aic$AICc)

# 6. Produce a plot of your own design to concisely visualize effe --------







