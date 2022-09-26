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

# Effect of Perch Height on Hindlimb-SVL Pattern
  anole.log%>%
    ggplot(aes(x=Ecomorph2,y=res.ph)) + geom_boxplot() 
# Effect of Perch Diameter on Hindlimb-SVL Pattern
  anole.log%>%
    ggplot(aes(x=Ecomorph2,y=res.pd)) + geom_boxplot() 
  

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
  
  
  # fit     delta           w
  # 1 -64.77956 10.746149 0.003241168
  # 2 -73.81081  1.714901 0.296354947
  # 3 -75.52571  0.000000 0.698551002

  # a phylogenetically corrected regression model that includes 
  # Perch Height and Diameter with traits evolving under BM is the best fit.
  # Both of the covariates together are a significant predictor of hindlimb 
  # length in a phylogenetic context. 


# 6. Produce a plot of your own design to concisely visualize effe --------
#Mutate anole.log to include phylogenetically best fit
  anole.log <- anole.log %>%
    mutate(phylo.res = residuals(pgls.BM.ph.pd))

  # Facet Grid Plot of PH
  p.ph.phylo <- anole.log %>%
    dplyr::select(PH,res.ph,phylo.res) %>%
    pivot_longer(cols = c("res.ph", "phylo.res"))%>%
    print %>%
    ggplot(aes(x = PH,y = value)) + 
    geom_boxplot() +
    facet_grid(name~.,scales = "free_y")+ylab("residual")

  # Facet Grid Plot of ArbPD
  p.pd.phylo <- anole.log %>% 
    dplyr::select(ArbPD,res.pd,phylo.res) %>%
    pivot_longer(cols = c("res.pd", "phylo.res"))%>%
    print%>%
    ggplot(aes(x = ArbPD,y = value)) + 
    geom_boxplot() +
  facet_grid(name~.,scales = "free_y") + ylab("residual")

  # Plot both Facet Grids next to each other using cowplot package
  plot_grid(p.ph.phylo,p.pd.phylo, labels = "AUTO")










