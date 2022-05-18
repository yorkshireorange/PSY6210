#### SET-UP ####

# packages
library(MVN)
library(here)
library(lavaan)
library(lavaanPlot)

# open data
load(here("data", "psy6210.RData"))

#### STUDY 2 ####

### MODEL 1 ###

# data check

#multivariate normality
mvn_test<-mvn(GamingAptitude[,c("Strategy","Task","Character","GlobalMovement",
                            "TaskSwitch","StroopTask","UpdatingTask",
                            "Knowledge","TimeSpent")], mvnTest = "royston")

mvn_test$multivariateNormality
sink(here("appendices", "app2", "mvn.txt"))
print(mvn_test$multivariateNormality)
sink()

# variance standardisation method
mod1 <- '
Metacog =~ NA*Strategy + Task + Character + GlobalMovement
WoMem =~ NA*TaskSwitch + StroopTask + UpdatingTask
CrystAb =~ NA*Knowledge + TimeSpent

# fixed variance
WoMem ~~ 1*WoMem
Metacog ~~ 1*Metacog 
CrystAb ~~ 1*CrystAb
'

cfa1 <-  cfa(mod1, data = GamingAptitude, std.lv = TRUE,
             se='robust.sem',test='satorra.bentler')

summary(cfa1, fit.measures=TRUE, standardized = TRUE)

sink(here("appendices", "app2", "cfa1.txt"))
print(summary(cfa1, fit.measures=TRUE, standardized = TRUE, modindices = TRUE))
sink()

inspect(cfa1,'r2')

# model plot
lavaanPlot(model = cfa1, 
           node_options = list(shape = "box", fontname = "Arial"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE)

# modification index
modindices(cfa1,sort=TRUE)

sink(here("appendices", "app2", "cfa1_mod_ind.txt"))
print(modindices(cfa1,sort=TRUE))
sink()

### MODEL 2 ###

# variance standardisation method
mod2 <- '
Metacog =~ NA*Strategy + Task + Character + GlobalMovement + Knowledge
WoMem =~ NA*TaskSwitch + StroopTask + UpdatingTask + TimeSpent

#fixed variance
Metacog ~~ 1*Metacog
WoMem ~~ 1*WoMem
'

cfa2 <-  cfa(mod2, data = GamingAptitude, std.lv = TRUE,
             se='robust.sem',test='satorra.bentler')

summary(cfa2, fit.measures=TRUE, standardized = TRUE, modindices = TRUE)
sink(here("appendices", "app2", "cfa2.txt"))
print(summary(cfa2, fit.measures=TRUE, standardized = TRUE, modindices = TRUE))
sink()

# model plot
lavaanPlot(model = cfa2, 
           node_options = list(shape = "box", fontname = "Arial"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE)

# save plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE) 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to=here("appendices", "app2", "plots"))

### CFA1 MODIFICATION ###

# variance standardisation method
mod1a <- '
Metacog =~ NA*Strategy + Task + Character + GlobalMovement
WoMem =~ NA*TaskSwitch + StroopTask + UpdatingTask
CrystAb =~ NA*Knowledge + TimeSpent

# fixed variance
WoMem ~~ 1*WoMem
Metacog ~~ 1*Metacog 
CrystAb ~~ 1*CrystAb

# post-hoc modifications (covariances)
Strategy ~~ Character
Strategy ~~ GlobalMovement
Character ~~ GlobalMovement
'

cfa1a <-  cfa(mod1a, data = GamingAptitude, std.lv = TRUE,
             se='robust.sem',test='satorra.bentler')

summary(cfa1a, fit.measures=TRUE, standardized = TRUE)

sink(here("appendices", "app2", "cfa1_mod.txt"))
print(summary(cfa1a, fit.measures=TRUE, standardized = TRUE, modindices = TRUE))
sink()

# model plot
lavaanPlot(model = cfa1a, 
           node_options = list(shape = "box", fontname = "Arial"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE)