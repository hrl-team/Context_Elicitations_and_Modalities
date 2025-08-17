###########################################################

# MODEL - Functions

###########################################################
# Functions needed for model fitting

# ============================================================
# MODEL FUNCTION
# ============================================================
# Fit RL model to a participant's trial-by-trial data.


# ----------Inputs----------------
#   par       : Vector of free parameters to be estimated
#   data      : learning and preference data combined (data frame) 
#               must contain rescaled outcomes (e.g outcome_ch.a, or outcome_ch.rel)
#   par_names : Names of free parameters
#   fix.par   : Named vector of fixed parameters
#   setUp     : List of model setup options
 
     
# ----------SetUp Options----------
#    fitOn   = "Learning"|"Preference"|"both"  
#               --> Minimize ll of Learning|Preference data only or both
#    lrType  = "single"
#                --> use a single lr for both absolute and relative Q-values
#    relative= "rel"     
#                --> use outcomes tagged "rel" for relative encoding
#                --> can add more options to allow to test different rel.encoding 


# ----------Outputs---------------
#   f         : Negative log-likelihood + priors penalty


# ----------Other Requirements----
#     Outcomes need to be in the (-1,1) format
# =========================================================

fit_RT_Relative <- function(par,
                           data,
                           par_names,
                           fix.par,
                           setUp){
  # ---------------------------------------------------------
  # 1. Check Parameter Priors For Free Parameters
  # ---------------------------------------------------------
  # If any prior is impossible, return large penalty

  priors<-
    data.frame(name=par_names, value=par) %>% 
    mutate(lik = case_when(name%in%c("beta")~dgamma(value, shape =1.2, scale = 5),
                           name%in%c("lr_a","lr_r","w")~dbeta(value,1.1,1.1),
                           TRUE~1)) 
  
  if(prod(priors$lik)==0){
    return(exp(100))
  }else{
    priors_sum <- log(prod(priors$lik))
  }
  
  # ---------------------------------------------------------
  # 2. Let Optimizer select a value for free parameters
  # ---------------------------------------------------------
  p <- par
  names(p)<- par_names
  
  p<- c(p,fix.par)
  
  if(setUp["lrType"]=="single"){
    p[["lr_r"]] = p[["lr_a"]]
  }
  
  # ---------------------------------------------------------
  # 3. Prepare Data
  # ---------------------------------------------------------
  # Convert the important data into vectors for speed
  # Arrange data by Task and Trial (So that they are in order of presentation)
  data<-
    data %>% 
    arrange(TaskName,trial)
  
  expType <- unique(data$expType)
  TaskName <- data$TaskName
  
  symID_ch <- data$symID_ch
  symID_u <- data$symID_u
  
  outcome_ch.a <- data$outcome_ch.a
  outcome_u.a <- data$outcome_u.a
  
  vals_on<- data$vals_on
  
  # Select relative coding type
  if(setUp["relative"]=="range"){
    outcome_ch.r <- data$outcome_ch.range
    outcome_u.r <- data$outcome_u.range
  }else if(setUp["relative"]=="rel"){
    outcome_ch.r <- data$outcome_ch.rel
    outcome_u.r <- data$outcome_u.rel
  }
  
  # ---------------------------------------------------------
  # 4. Initialize Q-values and Likelihood
  # ---------------------------------------------------------
  QA <- rep(0,n_distinct(c(symID_ch,symID_u)))
  QR <- rep(0,n_distinct(c(symID_ch,symID_u)))
  lik <- 0
  
  # ---------------------------------------------------------
  # 5. Trial Loop
  # ---------------------------------------------------------
  for(i in 1:nrow(data)){

      # ---Calculate Weighted Sum of Absolute And Relative Q-values ---

      M_ch <- p[["beta"]]*(QA[symID_ch[i]]*(1-p[["w"]]) +  p[["w"]]*QR[symID_ch[i]]) 
      M_u  <- p[["beta"]]*(QA[symID_u[i]]*(1-p[["w"]])   +  p[["w"]]*QR[symID_u[i]])  

    
      # --- Calculate Softmax Probability ---
      trial.lik <- log(logistic_simple(V_ch=M_ch,V_u=M_u))
    
      # --- Add the probability for the current trial to the LL ---
      if(TaskName[i]=="LearningTask"&setUp["fitOn"]!="Preference"){
        lik <- lik  + trial.lik
      
      }else if(TaskName[i]!="LearningTask"&setUp["fitOn"]!="Learning"){
      lik <- lik  +  trial.lik
      }
    
    
      # --- Value Update - Learning Task Only ---
    if(TaskName[i]=="LearningTask"){
      
      #Values for current trial
      ids <- c(symID_ch[i],symID_u[i])
      outcomes.a <- c(outcome_ch.a[i],outcome_u.a[i])
      outcomes.r <- c(outcome_ch.r[i],outcome_u.r[i])
      
      #Prediction Error Calculation
      delta.a <- outcomes.a - QA[ids]
      delta.r <- outcomes.r - QR[ids]
      
      #Q-value update
      QA[ids] <-   QA[ids] + p[["lr_a"]] * delta.a
      QR[ids] <-   QR[ids] + p[["lr_r"]] * delta.r 
    }
  }
  # ---------------------------------------------------------
  # Calculate and return log-likelihood + priors
  # ---------------------------------------------------------
  f = -1*(priors_sum+lik)
  return(f)

}
# =========================================================



# ============================================================
# WRAPPER FUNCTION
# ============================================================
# Added for ease of data processing
# Sets up optimx (function multistart) for sequential/parallel processing

# ----------Inputs----------------
#   pars      : matrix of starting points
#                  --> columns:names of free parameters, rows: starting points
#   bounds    : Data frame with bounds for each free parameter
#   pars_fix  : data frame with names + values for fixed parameters 
#   dataF     : data.frame with Learning + Preference data from all experiments/participants
#   model.fun : Name of function used for modelling
#   model     : String - this will be saved in a model column
#   selector  : If not parallel processing, by what the data should be grouped by
#   optMethod : Which optimisation method to use (from optimx)
#   setUp     : list with settings to be passed to the modelling function
#   parRun    : TRUE - run in parallel

# ----------Outputs---------------
#   fitted.data2
#       - Data set with best fitting parameter estimates for each participant/starting point
#       - Contains model name and calculated BIC/AIC
#       - Also includes info about the fitting process from multistart

# ============================================================

wrap_RT_Relative <- function(pars,pars_fix,
                         dataF,model.fun,
                         model,
                         setUp,
                         bounds,
                         selector,optMethod,
                         parRun = TRUE){
  
  # ---------------------------------------------------------
  # 1. Set up Parameter & Bounds 
  # ---------------------------------------------------------
  
  # sort pars in the same order as free (so that the appropriate limits apply to correct pars)
  pars2<- as.data.frame(pars) %>% as.matrix()
  par_names<- colnames(pars)
  
  bounds2 <- t(bounds) %>% as.data.frame()
  colnames(bounds2)<-c("lower","upper")
  
  # Define model name from a supplied string
  model.name <- model
  print(model.name)
  
  
  # ---------------------------------------------------------
  # 2. Split Data by Participant
  # ---------------------------------------------------------
  data.par<-
    as.data.frame(dataF) %>%
    mutate(partID = droplevels(partID)) %>% 
    ungroup() %>% 
    split.data.frame(.$partID)
  
  npart<- n_distinct(dataF$partID)
  ntrials <- unname(unlist(bind_rows(lapply(data.par,function(x) x %>% nrow()))))
  
  # ---------------------------------------------------------
  # 3. Fit Model (Parallel or Sequential)
  # ---------------------------------------------------------
  # Which modelling function to use
  m.fun<-match.fun(model.fun)
  
  # --------------Parallel ----------------------------------
  if(parRun==TRUE){
    fitted.data<-  
      foreach(i = 1:npart, .combine = rbind) %dofuture% {
        multistart(parmat = pars2,fn = m.fun, data = data.par[[i]],
                   lower = bounds2$lower,upper =bounds2$upper,
                   method =optMethod,
                   par_names = par_names,
                   fix.par = pars_fix,
                   setUp=setUp)
      } 
    
  }
  # --------------Sequential ----------------------------------
  else{
    fitted.data<-  
      as.data.frame(dataF) %>%
      group_by(across(all_of(selector))) %>%
      mutate(n=n()) %>% 
      group_by(across(all_of(c(selector,"n")))) %>% 
      do(multistart(parmat = pars2,fn = match.fun(model.fun), data = .,
                    lower = setUp$bounds[1,],upper =setUp$bounds[2,] ,
                    method =optMethod,
                    par_names =  par_names,
                    fix.par = pars_fix,
                    setUp=setUp))
    
    return(fitted.data)
  }
  
  # ---------------------------------------------------------
  # 4. Add Info & Compute AIC/BIC
  # ---------------------------------------------------------
  fitted.data2<-
    data.frame(partID=rep(names(data.par),each=nrow(pars)),
               n = rep(ntrials,each=nrow(pars)),
               fitted.data,
               sp=pars2,
               setUp = t(setUp)) %>% 
    mutate(model = model.name,
           BIC = 2*value +ncol(pars2)*log(n),
           AIC = 2*value +ncol(pars2)*2,
           optMethod = optMethod) 
  
  return(fitted.data2)
}
# ============================================================

# ============================================================
# FIND BEST SET PER STARTING POINT
# ============================================================

# best version
find.best<-
  function(data,part.exp){
    data.best<-
      data %>% 
      group_by(partID) %>% 
      mutate(min_val = min(value),
             best= value==min_val) %>% 
      filter(best==TRUE) %>% 
      group_by(partID) %>% 
      mutate(n=row_number()) %>% 
      filter(n==1) %>% 
      merge(.,part.exp,by="partID")
    
    return(data.best)
  }

# ============================================================

# =========================================================
# LOGISTIC FUNCTIONS
# =========================================================

logistic_simple <- function(V_ch,V_u){
  prob <- 1/(1+exp(V_u-V_ch))
}
