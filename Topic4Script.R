#####################################################
# Stat210 Prac/Workshop 4
# Code by Chris Cody. SN: 220209093

#####################################################
# Setup

# set no of significant figures and hide significance
# stars from output
options(digits = 6, show.signif.stars = T)

# import STAT210 functions script
source("Rfunctions.R")

# import librarys
library(lattice)

#####################################################
# Prac 4

# load data
samara.df <- read.table("samara.txt", header = T)

# plot data
samaraEDA <- xyplot(Velocity ~ Load | Tree,
  data = samara.df, layout = c(3, 1),
  panel = function(x, y) {
    panel.xyplot(x, y)
    panel.lmline(x, y)
  }
)
print(samaraEDA)

# declare Tree as a factor
# class(samara.df$Tree)
# levels(samara.df$Tree)
samara.df$Tree <- factor(samara.df$Tree)
# class(samara.df$Tree)
# levels(samara.df$Tree)

# Fit three models
# Interaction model
samara.mod1 <- lm(Velocity ~ Load * Tree, data = samara.df)
anova(samara.mod1)
# Main effects model
samara.mod2 <- lm(Velocity ~ Load + Tree, data = samara.df)
anova(samara.mod2)
# Simple linear regression model
samara.mod3 <- lm(Velocity ~ Load, data = samara.df)
anova(samara.mod3)

# compare models
anova(samara.mod1, samara.mod2, samara.mod3)

# stepwise regression: forward
samara.form_l <- formula(~1)
samara.form_u <- formula(~ Load * Tree, data = samara.df)

start.model <- lm(Velocity ~ 1, data = samara.df)
stepf.model <- step(
  start.model,
  direction = "forward",
  scope = list(
    lower = samara.form_l,
    upper = samara.form_u
  )
)

summary(stepf.model)

# stepwise regression: backward

startb.model <- lm(Velocity ~ Load * Tree, data = samara.df)
stepb.model <- step(
  startb.model,
  direction = "backward",
  scope = list(
    lower = samara.form_l,
    upper = samara.form_u
  )
)

summary(stepb.model)

anova(stepb.model)

betaCI(stepb.model)

#diagnostics plots for interaction model
par(mfrow=c(1,2))
plot(stepb.model,which=1:2)


# delete files for this prac
rm(list=ls(pattern="samara"))
rm(list=ls(pattern="st"))
