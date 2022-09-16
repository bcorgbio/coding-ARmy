#Loading Libraries and Data
library(tidyverse)

#Delete this?
setwd("~/Downloads/BIOL3140 Experimental Methods in Organismal Biology")

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")