#### STUDY 4 ####

### SET-UP ###

# packages
library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

# load data
load(here("data", "psy6210.RData"))

## STAGED MODELS

# Random 1: random intercept (RI) only
ra1 <- lmerTest::lmer(RT ~ 1 + (1|Participant_ID), data = SceneDec)
summary(ra1)
r.squaredGLMM(ra1)

# Random 2: SceneType + RI
ra2 <- lmerTest::lmer(RT ~ SceneType + (1|Participant_ID), data = SceneDec)
summary(ra2)
r.squaredGLMM(ra2)

# Random 3: SceneType + Age + RI
ra3 <- lmerTest::lmer(RT ~ SceneType + Age + (1|Participant_ID),
                      data = SceneDec)
summary(ra3)
r.squaredGLMM(ra3)

# Random 4: SceneType + Age + Consc + RI (full MLM)
ra4 <- lmerTest::lmer(RT ~ SceneType + Age + Consc + (1|Participant_ID),
                      data = SceneDec)
summary(ra4)
r.squaredGLMM(ra4)

anova(ra1, ra2, ra3, ra4)

# Random effect: participants' variation
ranef(ra4)

# text output
sink(here("appendices", "app4", "model_building.txt"))
print(summary(ra1))
print(summary(ra2))
print(summary(ra3))
print(summary(ra4))
sink()

sink(here("appendices", "app4", "model_comparison.txt"))
print(anova(ra1, ra2, ra3, ra4))
sink()

sink(here("appendices", "app4", "variance_change.txt"))
print(r.squaredGLMM(ra1))
print(r.squaredGLMM(ra2))
print(r.squaredGLMM(ra3))
print(r.squaredGLMM(ra4))
sink()

sink(here("appendices", "app4", "pps_variation.txt"))
print(ranef(ra4))
sink()