###########################################################

# Integration of Outcome Probability and Magnitude

###########################################################
# This script runs mixed-effects logistic regression models 
# (GLMMs) to compare additive vs multiplicative integration

# ============================================================
# REQUIREMENTS
# ============================================================
# Uncomment and install/load required packages before running:

# # models
# library(afex)
# library(glmmTMB)
# library(emmeans)
# 
# # model diagnostics
# library(DHARMa)
# library(performance)
# 
# # data manipulation
# library(tidyverse)
# library(patchwork)

# ============================================================
# LOAD DATA & FUNCTIONS
# ============================================================

# data
if(!exists("data")){
  data <- readRDS("data_RT_Anon_25.Rds")
}

# ============================================================
# Data Prepartion
# ============================================================
# - Standardizes predictors for comparability and calculates:
# - Differences in outcome magnitude, probability, and expected value
# - "Best" probability based on outcome valence (probability of a "good" outcome)

#---- All experiments  ------------------------------------------
MvsA.pref <- 
  data$preference %>% 
  filter(Day == 1) %>% 
  mutate(
    right_chosen = responseKey,
    # Reward for right/left option (normalized by 90)
    rew_right = ifelse(right_chosen == 1, rew_ch, rew_u) / 90,
    rew_left  = ifelse(right_chosen == 0, rew_ch, rew_u) / 90,
    # Probability for right/left option
    prob_right = ifelse(right_chosen == 1, prob_ch, prob_u) / 100,
    prob_left  = ifelse(right_chosen == 0, prob_ch, prob_u) / 100
  ) %>% 
  mutate(
    # Adjust probabilities based on reward sign (best option probability)
    prob_right.best = ifelse(sign(rew_right) > 0, prob_right, 1 - prob_right),
    prob_left.best  = ifelse(sign(rew_left)  > 0, prob_left, 1 - prob_left)
  ) %>% 
  mutate(
    # Differences between right and left options
    prob_rl.best = prob_right.best - prob_left.best,
    rew_rl       = rew_right - rew_left,
    ev_rl = rew_right * prob_right - rew_left * prob_left
  ) %>% 
  mutate(
    # Standardize all relevant predictors
    across(
      c(starts_with("rew_rl"), starts_with("prob_rl"),
        starts_with("ev_rl")),
      ~ datawizard::standardise(.),
      .names = "{.col}.s"
    )
  )


#---- Data split by experiment  ------------------------------------------
MvsA.pref.exp <- 
  MvsA.pref %>% 
  split.data.frame(.$expType,drop = TRUE) %>% 
  map(.,~.x %>% 
        mutate(
          across(
            c("rew_rl","prob_rl.best","ev_rl"),
            ~ datawizard::standardise(.),
            .names = "{.col}.s"
          )
        ))

# ============================================================
# GLMM1: Multiplicative Model
# ============================================================

#---- Fit model - All Exp ------------------------------------------
glm.mult.FO <- afex::mixed(right_chosen ~ expType*ev_rl.s+(1|partID),
                           data=MvsA.pref, method="LRT",family="binomial",
                           control=glmerControl(optimizer="bobyqa"))   

#---- Results + Diagnostics  ---------------------------------
DHARMa::simulateResiduals(glm.mult.FO $full_model) %>% plot()
print(glm.mult.FO)

#---- Post-hoc  ----------------------------------------------
emtrends(glm.mult.FO,~expType,var="ev_rl.s") %>% plot() + xlim(0,4) + theme_bw()

#---- Fit model -  Per expriment ------------------------------------
glm.mult.FO.exp <-
  lapply(MvsA.pref.exp, 
         function(x){
                    glm <- afex::mixed(right_chosen ~ ev_rl.s+(1|partID),
                    data=x, method="LRT",family="binomial",
                    control=glmerControl(optimizer="bobyqa"))
                    return(glm)
                    }
         )

lapply(glm.mult.FO.exp, function(x){DHARMa::simulateResiduals(x$full_model) %>% plot()})

# ============================================================
# GLMM2: Additive Model
# ============================================================

#---- Fit model ----------------------------------------------
glm.add.FO <- afex::mixed(right_chosen ~ expType*rew_rl.s + expType*prob_rl.best.s + (1|partID),
                          data=MvsA.pref, method="LRT",family="binomial",
                          control=glmerControl(optimizer="bobyqa"))

#---- Results + Diagnostics  ---------------------------------
DHARMa::simulateResiduals(glm.add.FO$full_model) %>% plot()
print(glm.add.FO)

#---- Post-hoc  ----------------------------------------------
# Contrasts
emtrends(glm.add.FO,"trt.vs.ctrl"~expType,var="rew_rl.s")
emtrends(glm.add.FO,"trt.vs.ctrl"~expType,var="prob_rl.best.s")

# Plots
emtrends(glm.add.FO,~expType,var="rew_rl.s") %>% plot() + coord_cartesian(xlim=c(0,3)) + ylab("")+
  theme_bw()+ xlab("Estimated Trend") + ggtitle("Magnitude Difference")+   scale_y_discrete(limits=c("E1","E2","E3","E4","E5","E6","E7"))+
  emtrends(glm.add.FO,~expType,var="prob_rl.best.s") %>% plot() + coord_cartesian(xlim=c(0,3))+ ylab("")+
  theme_bw()+xlab("Estimated Trend") +ggtitle("Probability Difference")+plot_layout(guides="collect")+ scale_y_discrete(limits=c("E1","E2","E3","E4","E5","E6","E7"))

#---- Fit model -  Per expriment ------------------------------------
glm.add.FO.exp <-
  lapply(MvsA.pref.exp, 
         function(x){
           glm <- afex::mixed(right_chosen ~ rew_rl.s + prob_rl.best.s+(1|partID),
                              data=x, method="LRT",family="binomial",
                              control=glmerControl(optimizer="bobyqa"))
           return(glm)
         }
  )

lapply(glm.add.FO.exp, function(x){DHARMa::simulateResiduals(x$full_model) %>% plot()})

# ============================================================
# Model Comparison
# ============================================================
anova(glm.add.FO,glm.mult.FO)

# x= additive, y=multiplicative
map2(glm.add.FO.exp,glm.mult.FO.exp, .f= ~ anova(.x, .y))

map2(glm.add.FO.exp,glm.mult.FO.exp, .f= ~ AIC(.x$full_model)- AIC(.y$full_model))
map2(glm.add.FO.exp,glm.mult.FO.exp, .f= ~ BIC(.x$full_model)- BIC(.y$full_model))

