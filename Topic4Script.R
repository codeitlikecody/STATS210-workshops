#####################################################
# Stat210 Prac/Workshop 4
# Code by Chris Cody. SN: 220209093

#####################################################
# Setup

# set no of significant figures and hide significance
# stars from output
options(digits = 6, show.signif.stars = F)

# import STAT210 functions script
source("Rfunctions.R")

# import librarys
library(lattice)

#####################################################
# Prac 4

# load data
samara.df <- read.table("samara.txt", header = T)

#plot data
samaraEDA <- xyplot(Velocity ~ Load | Tree,
  data = samara.df, layout = c(3, 1),
  panel = function(x, y) {
    panel.xyplot(x, y)
    panel.lmline(x, y)
  }
)
print(samaraEDA)

factor(samara.df$Tree)

# delete files for this prac
rm(samara.df, samaraEDA)
