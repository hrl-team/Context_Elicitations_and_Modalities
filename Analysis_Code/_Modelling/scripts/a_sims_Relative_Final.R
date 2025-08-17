###########################################################

# SIMULATION - SCRIPT

###########################################################
# Script used to simulate and plot data
# Mostly used for model falsification

# ============================================================
# REQUIREMENTS
# ============================================================
# Uncomment and install/load required packages before running:

# library(tidyverse)

# ============================================================
# LOAD DATA & FUNCTIONS
# ============================================================
# data
data <- readRDS("data_RT_Anon_25.Rds")

# fits
if(!exists("fitted.Relative.best")){
  fitted.Relative.best<- readRDS("fits_relative_best.Rds")
}

# Load functions needed for simulations and plotting
source("f_sim_Relative_Final.R")
source("f_plot_multSims_vs_data.R")

# ============================================================
# Prepare Data
# ============================================================
# Convert real data into simulation format
# (mainly adds 'cor'/'incor' columns and filters Day 1 data)

sim.data<-
  convert_realData(bind_rows(data$learning,data$preference)) %>% 
  filter(Day==1) %>% 
  filter(!expType%in%c("E4","E5")) %>% 
  group_by(expType, TaskName, ChoiceType.f, partID) %>%
  arrange(expType, partID, TaskName, trial) %>% 
  mutate(trial.cond = row_number()) %>% 
  mutate(ChoiceType = ChoiceType.f)

# ============================================================
# Define parameters 
# ============================================================

# Generate a parameter set from predefined distributions
# When not using parameters from participant's fits 

# set.seed(2112567)
# pars.test<- data.frame(
#   beta = runif(500,1,10),
#   lr_a = rbeta(500,1.5,3),
#   lr_r = rbeta(500,1.5,3),
#   w = rbeta(500,1.1,1.1))

# ============================================================
# Simulations
# ============================================================
# The following simulations use both participants trials structure
# and best fitting parameters to falsify models (best-case situation)

# Create empty list to store the simulations
set.seed(2112567)
sims.Relative<-list()

for(i in 1:1){
  # ---------- Absolute Model ---------
  sims.Relative[[1]]<-
    mapply(sim_RT_Relative,
           pars       =  data.frame(
                             beta = fitted.Relative.best[[1]]$beta,
                             lr_a = fitted.Relative.best[[1]]$lr_a,
                             lr_r = 0,
                             w    = 0) %>%
                             t() %>% 
                             as.data.frame(),
           pars_names = list(c("beta","lr_a","lr_r","w")), 
           setUp      = list(list(
                             fitOn    = "both",
                             lrType   = "single",
                             relative = "rel")), # has no effect if w=0
           model      = fitted.Relative.best[[1]]$model,
           data.part  = sim.data %>% split.data.frame(.$partID,drop = TRUE),
           expType    = fitted.Relative.best[[1]]$expType,
           SIMPLIFY   = FALSE) %>%
    bind_rows()
  
  # ---------- Relative Model ----------
  sims.Relative[[2]]<-
    mapply(sim_RT_Relative,
           pars       =  data.frame(
                            beta = fitted.Relative.best[[2]]$beta,
                            lr_a = fitted.Relative.best[[2]]$lr_a,
                            lr_r = 0,
                            w    = fitted.Relative.best[[2]]$w) %>%
                          t() %>% 
                          as.data.frame(),
           pars_names = list(c("beta","lr_a","lr_r","w")), 
           setUp      = list(list(
                             fitOn    = "both",
                             lrType   = "single",
                             relative = "rel")),
           model      = fitted.Relative.best[[2]]$model,
           data.part  = sim.data %>% split.data.frame(.$partID,drop = TRUE),
           expType    = fitted.Relative.best[[2]]$expType,
           SIMPLIFY   = FALSE) %>%
    bind_rows()
}

# ============================================================
# SAVE DATA
# ============================================================

saveRDS(sims.Relative,"sims_relative.Rds")

# ============================================================
# Plot - Falsification 
# ============================================================

# Plots multiple simulation vs empirical data
# changed model variable to have nicer labels in the plots
f.falsification<-
  plot_multSims_vs_data(
    sim=bind_rows(sims.Relative[c(1,2)]) %>% 
        mutate(model=factor(model,levels=c("abs","rel"),
                            labels=c("Absolute","Relative"))),
    data=sim.data,
    myPalette = c("#076d7e", "#F9AC00")
  )

# ============================================================