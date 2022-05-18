#### STUDY 3 ####

### SET-UP ###

# packages
library(here)
library(MVN)
library(lavaan)
library(lavaanPlot)

# load data
load(here("data", "psy6210.RData"))

### MODEL 1 ###

# multivariate normality
mvn_test<-mvn(GamingAptitude[,c("Strategy","Task","Character","GlobalMovement",
                                "TaskSwitch","StroopTask","UpdatingTask",
                                "ExploreExploit","EloScore", "BreakTime" )
                             ], mvnTest = "royston")

mvn_test$multivariateNormality
sink(here("appendices", "app3", "mvn.txt"))
print(mvn_test$multivariateNormality)
sink()


mod1 <- '
# LVs
Metacog =~ NA*Strategy + Task + Character + GlobalMovement
WoMem =~ NA*TaskSwitch + StroopTask + UpdatingTask

# mediator
ExploreExploit ~ a1*BreakTime + a2*Metacog

# outcome
EloScore ~ c1*BreakTime + c2*Metacog + WoMem + b*ExploreExploit

# indirect effect
ind1 := a1*b
ind2 := a2*b

# total effect
tot1 := c1 + (a1*b)
tot2 := c2 + (a2*b)

# fix LV variance
Metacog ~~ 1*Metacog
WoMem ~~ 1*WoMem
'

sem1 <- sem(mod1, data = GamingAptitude, std.lv = TRUE,
            meanstructure = TRUE)
summary(sem1, fit.measures=TRUE, standardized = TRUE, modindices = TRUE)
sink(here("appendices", "app3", "sem1.txt"))
print(summary(sem1, fit.measures=TRUE, standardized = TRUE, modindices = TRUE))
sink()

lavaanPlot(model = sem1, 
           node_options = list(shape = "box", fontname = "Arial"), 
           edge_options = list(color = "grey"), coefs = TRUE, 
           covs = TRUE, stand = FALSE)

### INVARIANCE ###

# configural invariance
sem_civ <- sem(mod1, data = GamingAptitude, group='Sex', std.lv = TRUE,
               meanstructure = TRUE)
summary(sem_civ, fit.measures=TRUE, standardized = TRUE)

# metric invariance
sem_miv <- sem(mod1, data = GamingAptitude, group='Sex', group.equal='loadings',
             std.lv = TRUE, meanstructure = TRUE)
summary(sem_miv, fit.measures=TRUE, standardized = TRUE)

summary(compareFit(sem_civ,sem_miv))
sink(here("appendices", "app3", "metric_inv.txt"))
print(summary(compareFit(sem_civ,sem_miv)))
sink()
