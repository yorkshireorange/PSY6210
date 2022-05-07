#### SET-UP ####

# packages
library(tidyverse)
library(ggplot2)
library(here)

# open data
load(here("data", "psy6210.RData"))
#### STUDY 1 ####

### MODEL 1 ###

# relevel categorical vars
GamingAptitude$Sex <- relevel(GamingAptitude$Sex, ref = "0")
GamingAptitude$SES <- relevel(GamingAptitude$SES, ref = "1")

# build glm
mod1 <- lm(Interest ~ ScreenTime + Extraversion + SES + Sex, data = GamingAptitude)
summary(mod1)
sink(here("appendices", "app1", "model1.txt"))
print(summary(mod1))
sink()

# compare to the null model
mod0 <- lm(Interest ~ 1, data = GamingAptitude)
summary(mod0)
anova(mod0,mod1)

sink(here("appendices", "app1", "null_model.txt"))
print(anova(mod0,mod1))
sink()

# inspect stdz residuals
car:: residualPlot(mod1, type='rstandard')

# Linearity / additivity of cat IVs and the DV
p1 <- ggplot(GamingAptitude, aes(x = ScreenTime, y=Interest))
p1 + geom_smooth(method = "lm", se=FALSE) + 
  geom_point()

p2 <- ggplot(GamingAptitude, aes(x = Extraversion, y=Interest))
p2 + geom_smooth(method = "lm", se=FALSE) + 
  geom_point()

# autocorrelation
stats::acf(resid(mod1))

# influential cases dg
car::qqPlot(mod1)
car:: influenceIndexPlot(mod1, vars=c("studentized", "hat", "Cook", "Bonf" ))

# Colinearity
col1 <- cor(GamingAptitude[,c('ScreenTime', 'Extraversion')], use = "complete.obs",
    method = "pearson")
col1

sink(here("appendices", "app1", "col1.txt"))
print(col1)
sink()

### MODEL 1A ###

# remove outliers
data_rev <- GamingAptitude[-c(366,582),]

# relevel categorical vars
data_rev$Sex <- relevel(data_rev$Sex, ref = "0")
data_rev$SES <- relevel(data_rev$SES, ref = "1")

# build glm
mod1a <- lm(Interest ~ ScreenTime + Extraversion + SES + Sex, data = data_rev)
summary(mod1a)

sink(here("appendices", "app1", "model1a.txt"))
print(summary(mod1a))
sink()

# inspect stdz residuals
car:: residualPlot(mod1a, type='rstandard')

# influential cases dg
car::qqPlot(mod1a)
car:: influenceIndexPlot(mod1a, vars=c("studentized", "hat", "Cook", "Bonf" ))

# save plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE) 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to=here("appendices", "app1", "plots"))
