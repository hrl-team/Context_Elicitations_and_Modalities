############################################################

#  Model Fitting 

############################################################
# Purpose: Fit and compare models to the behavioural data

# ============================================================
# REQUIREMENTS
# ============================================================
# Uncomment and install/load required packages before running:

# library(optimx)
# library (doFuture)
# library(tidyverse)
# library (bmsR)

# ============================================================
# LOAD DATA & FUNCTIONS
# ============================================================
data_path <- "data_RT_Anon_25.Rds"
data <- readRDS(data_path) 

# Load functions needed for model fitting
source("f_model_Relative.R")

# Load functions needed for model selection
source("f_BMS.R")

# ============================================================
# STARTING POINTS & BOUNDS
# ============================================================
set.seed(213140)

# Random starting points for optimization
sp.points <-
  data.frame(beta  = runif(5,1,6), 
             lr_a= rbeta(5,1,4),
             lr_r = rbeta(5,1,4),
             w = rbeta(5,1,4))%>% 
  as.matrix()

# Parameter bounds (min, max)
bounds <-
  data.frame(beta  = c(0,30),
             lr_a= c(0,1),
             lr_r = c(0,1),
             w = c(0,1))

# ============================================================
# DATA PREPARATION FOR FITTING
# ============================================================
# Removes E4 and E5 experiments (not suitable for this kind of model)
# Removes data from Day 2
# Computes absolute and relative outcomes

data.fit.noDesc<-
  data$learning %>% 
  filter(!expType%in%c("E4","E5")) %>% 
  group_by(ChoiceType.f,TaskName,expType) %>% 
  mutate(minChT = min(outcome_ch,outcome_u), # minimum outcome for choice type
         maxChT = max(outcome_ch,outcome_u)  # maximum outcome for choice type
         ) %>% 
  ungroup() %>% 
  mutate(outcome_ch.a = outcome_ch/90,                  # absolute chosen outcome
         outcome_u.a  = outcome_u/90,                   # absolute unchosen outcome
         outcome_ch.rel = sign(outcome_ch-outcome_u),   # relative chosen outcome
         outcome_u.rel = sign(outcome_u-outcome_ch)     # relative unchosen outcome
         ) %>% 
  bind_rows(.,data$preference) %>%
  filter(!expType%in%c("E4","E5")) %>% 
  filter(Day==1) %>% 
  mutate(vals_on = ifelse(vals_on==1,1,0))

# Generate a subset of partIDs for testing
# test.part<-  
#   data.fit.noDesc %>%  
#   distinct(partID,expType) %>% 
#   group_by(expType) %>% 
#   sample_n(1)

# data.fit.noDesc %>% filter(partID%in%test.part$partID)

# ============================================================
# MODEL FITTING
# ============================================================
# store results
fitted.Relative <- list()         

# Fit in parallel (each worker = one session)
plan("cluster",workers=25)

for(i in 1:1){
  
  # ---------- Absolute Model ---------
  print(Sys.time())
  fitted.Relative[[1]]<-
    wrap_RT_Relative(
        model.fun = "fit_RT_Relative",
        dataF     = data.fit.noDesc,
        pars      = sp.points[1:5,c("beta","lr_a")],
        bounds    = bounds[,c("beta","lr_a")],
        pars_fix  = data.frame(w=0, lr_r=0) %>% as.vector(), #Set values for fixed parameters
        selector  = c("partID","expType"),
        optMethod = "nlminb",
        setUp     = list(
          fitOn   = "both",   # fit on both learning & preference tasks
          lrType  = "single", # use a single lr for absolute and relative
          relative= "rel"     # use outcomes tagged "rel" for relative encoding (no impact if w=0)
         ),
        model     = "abs",
        parRun    = TRUE      # run in paralel or not
      )
  
  # ---------- Relative Model ----------
  print(Sys.time())
  fitted.Relative[[2]]<-
    wrap_RT_Relative(
      model.fun = "fit_RT_Relative",
      dataF     = data.fit.noDesc,
      pars      = sp.points[1:5,c("beta","lr_a","w")],
      bounds    = bounds[,c("beta","lr_a","w")],
      pars_fix  = data.frame(lr_r=0) %>% as.vector(),
      selector  = c("partID","expType"),
      optMethod = "nlminb",
      setUp     = list(
        fitOn   = "both",   # fit on both learning & preference tasks
        lrType  = "single", # use a single lr for absolute and relative
        relative= "rel"     # use outcomes tagged "rel" for relative encoding
      ),
      model     = "rel",
      parRun    = TRUE      # run in paralel or not
    )
}

# Revert to sequential processing
plan("sequential")


# ============================================================
# CLEAN
# ============================================================

# Extract participant/experiment mapping
part.exp <-
  bind_rows(data.fit.noDesc) %>% 
  ungroup %>%
  distinct(partID,expType)

# Get the best fitting iteration (ie. best starting point for each model/participant)
fitted.Relative.best<-
  lapply(fitted.Relative ,function(x) x<-find.best(x,part.exp))

# Name each list with the name of the model
names<-
  unlist(lapply(fitted.Relative.best ,function(x) x<-unique(x$model)))

names(fitted.Relative.best)<-names

# ============================================================
# SAVE DATA
# ============================================================

 saveRDS(fitted.Relative.best,"fits_relative_best.Rds")

# ============================================================
# COMPARE MODELS
# ============================================================
# Bayesian Model Selection (BMS) by experiment 

bms<-
  bind_rows(fitted.Relative.best) %>% 
  mutate(expType=factor(expType,levels=c("E1","E2","E3","E6","E7"))) %>% 
  group_by(expType) %>%
  do(bms=do.bms(.,method="AIC"))


# ============================================================
# TABLE WITH PARAMETER ESTIMATES PER MODEL
# ============================================================
table.par.mean<-
  lapply(fitted.Relative.best[c("rel")],function(x) x<-x %>% 
           pivot_longer(cols=any_of(c("beta","lr_a","w")),
                        names_to = "parameter", values_to = "par_value") %>% 
           mutate(parameter = factor(parameter,levels=c("beta","lr_a","lr_r","w","d"))) %>% 
           mutate(expType= factor(expType,levels = c("E1","E2","E3","E6","E7"))) %>% 
           group_by(expType,parameter) %>% 
           summarise(mean_val = mean(par_value)) %>% 
           pivot_wider(names_from = parameter,values_from = mean_val))

table.par.sd<-
  lapply(fitted.Relative.best[c("rel")],function(x) x<-x %>% 
           pivot_longer(cols=any_of(c("beta","lr_a","w")),
                        names_to = "parameter", values_to = "par_value") %>% 
           mutate(parameter = factor(parameter,levels=c("beta","lr_a","lr_r","w","d"))) %>% 
           mutate(expType= factor(expType,levels = c("E1","E2","E3","E6","E7"))) %>% 
           group_by(expType,parameter) %>% 
           summarise(sd_val = sd(par_value)) %>% 
           pivot_wider(names_from = parameter,values_from = sd_val))

# ============================================================
# STAT TEST: W parameter across experiments
# ============================================================
test.w.glm <- glmmTMB::glmmTMB(w~expType,
                               data=fitted.Relative.best[["freq"]],
                               family=glmmTMB::beta_family()) 
car::Anova(test.w.glm, type=3)
summary(test.w.glm)
performance::check_model(test.w.glm)
DHARMa::simulateResiduals(test.w.glm) %>% plot()

emmeans::emmeans(test.w.glm,"trt.vs.ctrl"~expType)
emmeans::emmeans(test.w.glm,"pairwise"~expType)


# ============================================================

