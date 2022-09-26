# Module 3 Project - Coding aRmy (Justin L, Katherine Q, Liz A, Michael B)

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
  library(phangorn)
  library(cowplot)})

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
# Perch Height
  anole.log.ph.lm <- lm(HTotal~SVL+PH, anole.log)

# Perch Diameter
  anole.log.pd.lm <- lm(HTotal~SVL+ArbPD, anole.log)

# 3. Explore how perch diameter and height affects Hindlimb-SVL relationship --------
  # Add Residuals
  anole.log <- anole.log %>%
    mutate(res.ph = residuals(anole.log.ph.lm), 
          res.pd = residuals(anole.log.pd.lm))
  #Effect of Perch Height and Diameter
  anole.log%>%
    dplyr::select(Ecomorph2,res.ph,res.pd)%>%
    pivot_longer(cols=c("res.ph","res.pd"))%>%
    print%>%
    ggplot(aes(x=Ecomorph2,y=value)) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", size=3)+
    facet_grid(name~.,scales = "free_y")+ylab("residual")

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

  # A PGLS model with the hindlimb-SVL relationship + perch height + perch diameter
  pgls.BM.ph.pd <- gls(HTotal~SVL + PH + ArbPD, 
                      correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                      data= anole.log, method = 'ML')

# 5.  Assess the fit of each of the three models using AICc and AICw --------
  anole.phylo.aic <- MuMIn::AICc(pgls.BM.ph, pgls.BM.pd, pgls.BM.ph.pd)
  aicw(anole.phylo.aic$AICc)
  
  #      fit     delta           w
  # 1 -64.77956 10.746149 0.003241168
  # 2 -73.81081  1.714901 0.296354947
  # 3 -75.52571  0.000000 0.698551002

  # a phylogenetically corrected regression model that includes 
  # Perch Height and Diameter with traits evolving under BM is the best fit.
  # Both of the variables together are a significant predictor of hindlimb 
  # length in a phylogenetic context. The model with both variables has a delta value of 0. 


# 6. Produce a plot of your own design to concisely visualize effe --------
  #Mutate anole.log to include phylogenetically best fit
  anole.log <- anole.log %>%
    mutate(phylo.res = residuals(pgls.BM.ph.pd))
  
  #Facet grid with phylo.res, res.ph, res.pd
  anole.log %>% 
    dplyr::select(Ecomorph2,res.ph,res.pd,phylo.res) %>%
    pivot_longer(cols = c("res.ph","res.pd", "phylo.res"))%>%
    print%>%
    ggplot(aes(x = Ecomorph2,y = value)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", size=3)+
    facet_grid(name~.,scales = "free_y") + ylab("residual")











