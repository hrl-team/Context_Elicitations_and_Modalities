###########################################################

# RECOVERY - SCRIPT

###########################################################
# Script used for parameter and model recovery

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

# data
if(!exists("data")){
  data <- readRDS("data_RT_Anon_25.Rds")
}


# Load functions needed for model fitting and simulations
source("f_model_Relative.R")
source("f_sim_Relative_Final.R")


# ============================================================
# Generate Parameters for Simulations
# ============================================================

set.seed(2112567)
pars.test<- data.frame(
  beta = runif(500,1,8),
  lr_a = rbeta(500,1.5,3),
  lr_r = rbeta(500,1.5,3),
     w = rbeta(500,1.1,1.1))


# ============================================================
# Simulations
# ============================================================
# The following simulations generate both the trial structure
# and parameters from scratch.

# Create empty list to store the simulations
set.seed(2112567)

sims.Rec<-list()

for(i in 1:1){
  # ---------- Absolute Model ---------
  print(Sys.time())
  sims.Rec[[1]]<-
    mapply(sim_RT_Relative,
           pars       =  data.frame(
                            beta = pars.test$beta,
                            lr_a = pars.test$lr_a,
                            lr_r = 0,
                            w    = 0) %>%
                        t() %>% 
                        as.data.frame(),
           pars_names = list(c("beta","lr_a","lr_r","w")), 
           setUp      = list(list(
                           fitOn    = "both",
                           lrType   = "single",
                           relative = "rel"
                         )),
           model="abs",
           expType=rep(c("E1","E2","E3","E6","E7"),100),
           SIMPLIFY=FALSE) %>%
    bind_rows()
  
  # ---------- Relative Model ----------
  print(Sys.time())
  sims.Rec[[2]]<-
    mapply(sim_RT_Relative,
           pars       = data.frame(
                            beta = pars.test$beta,
                            lr_a = pars.test$lr_a,
                            lr_r = 0,
                            w = pars.test$w) %>%
                        t() %>% 
                        as.data.frame(),
           pars_names = list(c("beta","lr_a","lr_r","w")), 
           setUp      = list(list(
                            fitOn = "both",
                            lrType = "single",
                            relative = "rel"
                            )),
           model      = "rel",
           expType   = rep(c("E1","E2","E3","E6","E7"),100),
           SIMPLIFY   = FALSE) %>%
    bind_rows()
}

# ============================================================
# Prepare Simulated Data for Fitting
# ============================================================

sims.to.fit<-
  lapply(sims.Rec,function(x) x<-
           x %>% 
           mutate(partID=as.factor(partID)) %>% 
          filter(!expType%in%c("E4","E5")) %>% 
          group_by(ChoiceType,TaskName,partID,expType) %>% 
          mutate(minChT = min(outcome_ch,outcome_u),
                 maxChT = max(outcome_ch,outcome_u)) %>% 
          ungroup() %>% 
          mutate(outcome_ch.a = outcome_ch/90,
                 outcome_u.a  = outcome_u/90,
                 outcome_ch.rel= sign(outcome_ch-outcome_u),
                 outcome_u.rel = sign(outcome_u-outcome_ch))%>% 
          filter(!expType%in%c("E4","E5")) %>% 
          mutate(vals_on = 0)%>% 
          arrange(expType,partID,TaskName,trial)) 

         
# ============================================================
# Specify Starting Points and Bounds
# ============================================================
set.seed(213140)

# Starting points for optimization
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
# Model Fitting
# ============================================================

# Create empty list to store the simulations
fitted.Recovery.Relative <- list()

# Fit in parallel (each worker = one session)
plan("cluster",workers=25)
for(i in 1:1){
  
  # ----Absolute Model: Absolute Data---------
  print(Sys.time())
  fitted.Recovery.Relative[[1]]<-
    wrap_RT_Relative(
      model.fun = "fit_RT_Relative",
      dataF     = sims.to.fit[[1]], 
      pars      = sp.points[1:5,c("beta","lr_a")],
      bounds    = bounds[,c("beta","lr_a")],
      pars_fix  = data.frame(w=0, lr_r=0) %>% as.vector(), 
      selector  = c("partID","expType"),
      optMethod = "nlminb",
      setUp     = list(
        fitOn   = "both",   
        lrType  = "single", 
        relative= "rel"    
      ),
      model     = "abs",
      parRun    = TRUE    
    )%>% 
    mutate(data.source = "abs")
  
  # ----Relative Model: Relative Data---------
  print(Sys.time())
  fitted.Recovery.Relative[[2]]<-
    wrap_RT_Relative(
      model.fun = "fit_RT_Relative",
      dataF     = sims.to.fit[[2]],
      pars      = sp.points[1:5,c("beta","lr_a","w")],
      bounds    = bounds[,c("beta","lr_a","w")],
      pars_fix  = data.frame(lr_r=0) %>% as.vector(),
      selector  = c("partID","expType"),
      optMethod = "nlminb",
      setUp     = list(
        fitOn   = "both",   
        lrType  = "single", 
        relative= "rel"     
      ),
      model     = "rel",
      parRun    = TRUE      
    )%>% 
    mutate(data.source = "rel")
  
  # ----Absolute Model: Relative Data---------
  print(Sys.time())
  fitted.Recovery.Relative[[3]]<-
    wrap_RT_Relative(
      model.fun = "fit_RT_Relative",
      dataF     = sims.to.fit[[2]], 
      pars      = sp.points[1:5,c("beta","lr_a")],
      bounds    = bounds[,c("beta","lr_a")],
      pars_fix  = data.frame(w=0, lr_r=0) %>% as.vector(), 
      selector  = c("partID","expType"),
      optMethod = "nlminb",
      setUp     = list(
        fitOn   = "both",   
        lrType  = "single", 
        relative= "rel"    
      ),
      model     = "abs",
      parRun    = TRUE      
    )%>% 
    mutate(data.source = "rel")
  
  # ----Relative Model: Absolute  Data---------
  print(Sys.time())
  fitted.Recovery.Relative[[4]]<-
    wrap_RT_Relative(
      model.fun = "fit_RT_Relative",
      dataF     = sims.to.fit[[1]],
      pars      = sp.points[1:5,c("beta","lr_a","w")],
      bounds    = bounds[,c("beta","lr_a","w")],
      pars_fix  = data.frame(lr_r=0) %>% as.vector(),
      selector  = c("partID","expType"),
      optMethod = "nlminb",
      setUp     = list(
        fitOn   = "both",   
        lrType  = "single",
        relative= "rel"     
      ),
      model     = "rel",
      parRun    = TRUE      
    ) %>% 
    mutate(data.source = "abs")
  
}
plan("sequential")

# ============================================================
# CLEAN
# ============================================================
# Extract participant/experiment mapping
sim.exp <-
  bind_rows(sims.to.fit)%>%
  ungroup %>%
  distinct(partID,expType)

# Get the best fitting iteration 
best.rec <- mapply(find.best,
                   data     = fitted.Recovery.Relative,
                   part.exp = list(sim.exp),
                   SIMPLIFY = FALSE)

# ============================================================
# Save
# ============================================================

par_recovery <- list(sims      = sims.to.fit, 
                     fits      = fitted.Recovery.Relative, 
                     best_fits = best.rec)
  
saveRDS(par_recovery,"recovery_relative_25.Rds")
  
# ============================================================
# Parameter Recovery Plot
# ============================================================
# process original parameter values
df.sims<-
  bind_rows(sims.to.fit[[2]]) %>%
  distinct(beta,lr_a,w,partID,model) %>% 
  #filter(TaskName=="LearningTask"&trial==1) %>%
  #dplyr::select(beta,lr_a,w,partID,model) %>% 
  pivot_longer(cols=c(beta,lr_a,w),names_to = "parameter", values_to = "real")

# process recovered parameter values
df.fits<- 
  bind_rows(best.rec[[2]]) %>% 
  mutate(partID=as.numeric(partID)) %>% 
  pivot_longer(cols=c(beta,lr_a,w),names_to = "parameter", values_to = "recovered") %>% 
  merge(.,df.sims,by=c("partID","parameter")) 

# plot
ggplot(aes(x=real,y=recovered),data=df.fits %>% 
         mutate(parameter = factor(parameter, levels=c('beta', 'lr_a','w'))))+
  geom_point(alpha=0.4)+
  ggh4x::facet_grid2(parameter~expType,scales = "free", independent = "x",axes="all")+
  geom_abline(slope=1)+
  geom_smooth(method="lm",se = FALSE,fullrange=TRUE,color="#0B889D")+
  theme_classic()+
  theme(legend.position="none",
        panel.background = element_rect(fill = "white", colour = "black"))+
  ggh4x::facetted_pos_scales(
    y = list(
      parameter %in% c("beta") ~ ylim(0,10),
      parameter %in% c("w") ~ ylim(0,1),
      parameter %in% c("lr_a") ~ ylim(0,1)
    ),
    x = list(
      parameter %in% c("beta") ~ xlim(0,10),
      parameter %in% c("w") ~ xlim(0,1),
      parameter %in% c("lr_a") ~ xlim(0,1)
    )
  )+
  ggpubr::stat_cor(color="#0B889D",r.digits = 3,p.accuracy = 0.001,)+
  xlab("Real Value")+ylab("Recovered Value")


# ============================================================
# Model Recovery Plot
# ============================================================
# Confusion Matrix
confusion<-
  bind_rows(best.rec) %>% 
  select(expType,partID,model,AIC,data.source) %>% 
  mutate(real = data.source) %>% 
  mutate(recovered = model) %>% 
  group_by(partID,expType,real) %>% 
  mutate(best.AIC = min(AIC),
         is.best = ifelse(AIC==best.AIC,1,0)) %>% 
  filter(is.best==1) %>% 
  group_by(expType,real,recovered,.drop = FALSE) %>%
  summarise(n=n()) %>% 
  group_by(real,expType) %>% 
  mutate(perc=round(n/sum(n),3)) %>% 
  arrange(real,recovered) 

# Inversion Matrix
inversion<-
  bind_rows(best.rec) %>% 
  select(expType,partID,model,AIC,data.source) %>% 
  mutate(real = data.source) %>% 
  mutate(recovered = model) %>% 
  group_by(partID,expType,real) %>% 
  mutate(best.AIC = min(AIC),
         is.best = ifelse(AIC==best.AIC,1,0)) %>% 
  filter(is.best==1) %>% 
  group_by(expType,real,recovered,.drop = FALSE) %>%
  summarise(n=n()) %>% 
  group_by(recovered,expType) %>% 
  mutate(perc=round(n/sum(n),3)) %>% 
  arrange(real,recovered) 

# plot
ggplot(aes(x=recovered,y=real,fill=perc),data=confusion)+geom_raster()+
  facet_wrap(~expType)+geom_label(color="white",aes(label=perc),label.size = 0)+
  xlab("recovered model")+ylab("generative model")+ggtitle("Confusion Matrix")+
  theme(legend.position = "none") +
  theme_classic() + theme(legend.position = "none",panel.background = element_rect(fill = "white", colour = "black"))+

ggplot(aes(x=recovered,y=real,fill=perc),data=inversion)+geom_raster()+
  facet_wrap(~expType)+geom_label(color="white",aes(label=perc),label.size = 0)+
  xlab("recovered model")+ylab("generative model")+ggtitle("Inversion Matrix") +
  theme(legend.position = "none") +
  theme_classic() + theme(legend.position = "none",panel.background = element_rect(fill = "white", colour = "black"))
  
#######################

confusion.simple<-
  bind_rows(best.rec) %>% 
  select(expType,partID,model,AIC,data.source) %>% 
  mutate(real = data.source) %>% 
  mutate(recovered = model) %>% 
  group_by(partID,expType,real) %>% 
  mutate(best.AIC = min(AIC),
         is.best = ifelse(AIC==best.AIC,1,0)) %>% 
  filter(is.best==1) %>% 
  group_by(real,recovered,.drop = FALSE) %>%
  summarise(n=n()) %>% 
  group_by(real) %>% 
  mutate(perc=round(n/sum(n),3)) %>% 
  arrange(real,recovered) 

inversion.simple<-
  bind_rows(best.rec) %>% 
  select(expType,partID,model,AIC,data.source) %>% 
  mutate(real = data.source) %>% 
  mutate(recovered = model) %>% 
  group_by(partID,real) %>% 
  mutate(best.AIC = min(AIC),
         is.best = ifelse(AIC==best.AIC,1,0)) %>% 
  filter(is.best==1) %>% 
  group_by(real,recovered,.drop = FALSE) %>%
  summarise(n=n()) %>% 
  group_by(recovered) %>% 
  mutate(perc=round(n/sum(n),3)) %>% 
  arrange(real,recovered) 

ggplot(aes(x=recovered,y=real,fill=perc),data=confusion.simple)+geom_raster()+
  geom_label(color="white",aes(label=perc),label.size = 0)+
  xlab("recovered model")+ylab("generative model")+ggtitle("Confusion Matrix")+
  theme(legend.position = "none") +
  theme_classic() + theme(legend.position = "none",panel.background = element_rect(fill = "white", colour = "black"))+
  
ggplot(aes(x=recovered,y=real,fill=perc),data=inversion.simple)+geom_raster()+
  geom_label(color="white",aes(label=perc),label.size = 0)+
  xlab("recovered model")+ylab("generative model")+ggtitle("Inversion Matrix") +
  theme(legend.position = "none") +
  theme_classic() + theme(legend.position = "none",panel.background = element_rect(fill = "white", colour = "black"))
