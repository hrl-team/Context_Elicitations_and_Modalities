###########################################################

# SIMULATION - Functions

###########################################################
# Functions needed for simulating data from models

# =========================================================
# Generate Outcomes
# =========================================================
# Generates the task structure (symbols, outcomes etc.)

# ----------Inputs----------------
#   type      : Learning|Preference (which Task to simulate)
#   expType   : E1|E2|E3|E6|E7 (which experiment to simulate)

# ----------Outputs---------------
#   generated.outcomes.interleaved: 
#     --> data.frame with all relevant trial-level infO (which symbols and outcomes)
# =========================================================

generate.outcomes <- function(type,expType){
  
  # -------------------------------------------------------
  # Learning Task — E1–E3 (Pair by Valence + Outcome Magnitude )
  # -------------------------------------------------------
  if(type=="Learning" & expType%in%c("E1","E2","E3")){
    
    # Correct and incorrect symbol IDs
    symID_cor <- c(rep(2,28),rep(4,28),rep(6,28),rep(8,28))
    symID_incor <- c(rep(1,28),rep(3,28),rep(5,28),rep(7,28))
    
    # Rewards for correct / incorrect responses
    rew_cor <- c(rep(-90,28),rep(-10,28),rep(10,28),rep(90,28))
    rew_incor <- c(rep(-90,28),rep(-10,28),rep(10,28),rep(90,28))
    
    # Reward probabilities (percent)
    prob_cor <- c(rep(25,28),rep(25,28),rep(75,28),rep(75,28))
    prob_incor <- c(rep(75,28),rep(75,28),rep(25,28),rep(25,28))
    
    # Task Label
    TaskName<-"LearningTask"

    # -------------------------------------------------------
    # Learning Task —  E6 (Pair by Valence + Probability)
    # -------------------------------------------------------
  }else if(type=="Learning" & expType%in%c("E6")){
    
    # Correct and incorrect symbol IDs
    symID_cor <- c(rep(3,28),rep(4,28),rep(7,28),rep(8,28))
    symID_incor <- c(rep(1,28),rep(2,28),rep(5,28),rep(6,28))
    
    # Rewards for correct / incorrect responses
    rew_cor <- c(rep(-10,28),rep(-10,28),rep(90,28),rep(90,28))
    rew_incor <- c(rep(-90,28),rep(-90,28),rep(10,28),rep(10,28))
    
    # Reward probabilities (percent)
    prob_cor <- c(rep(75,28),rep(25,28),rep(25,28),rep(75,28))
    prob_incor <- c(rep(75,28),rep(25,28),rep(25,28),rep(75,28))
    
    # Task Label
    TaskName<-"LearningTask"

    # -------------------------------------------------------
    # Learning Task — E7 (All possible pairs)
    # -------------------------------------------------------
  }else if(type=="Learning" & expType%in%c("E7")){
    
    # Correct and incorrect symbol IDs
    symID_cor <- rep(c(rep(8,7),rep(7,6),rep(6,5),rep(5,4),rep(4,3),rep(3,2),2),4)
    symID_incor <- rep(c(seq(7,1),seq(6,1),seq(5,1),seq(4,1),seq(3,1),seq(2,1),1),4)
    
    # Rewards for correct / incorrect responses
    rew_cor <- rep(c(rep(90,7),rep(90,6),rep(10,5),rep(10,4),rep(-10,3),rep(-10,2),-90),4)
    rew_incor <- rep(c(c(90,10,10,-10,-10,-90,-90),c(10,10,-10,-10,-90,-90),c(10,-10,-10,-90,-90),c(-10,-10,-90,-90),c(-10,-90,-90),c(-90,-90),-90),4)
    
    # Reward probabilities (percent)
    prob_cor <- rep(c(rep(75,7),rep(25,6),rep(75,5),rep(25,4),rep(25,3),rep(75,2),25),4)
    prob_incor <- rep(c(c(25,75,25,25,75,25,75),c(75,25,25,75,25,75),c(25,25,75,25,75),c(25,75,25,75),c(75,25,75),c(25,75),75),4)
    
    # Task Label
    TaskName<-"LearningTask"
    
    # -------------------------------------------------------
    # Preference Task (All possible pairs)
    # -------------------------------------------------------
   }else if(type=="Preference"){
    
    # Correct and incorrect symbol IDs
    symID_cor <- c(rep(8,7),rep(7,6),rep(6,5),rep(5,4),rep(4,3),rep(3,2),2)
    symID_incor <- c(seq(7,1),seq(6,1),seq(5,1),seq(4,1),seq(3,1),seq(2,1),1)
   
    # Rewards for correct / incorrect responses
    rew_cor <- c(rep(90,7),rep(90,6),rep(10,5),rep(10,4),rep(-10,3),rep(-10,2),-90)
    rew_incor <- c(c(90,10,10,-10,-10,-90,-90),c(10,10,-10,-10,-90,-90),c(10,-10,-10,-90,-90),c(-10,-10,-90,-90),c(-10,-90,-90),c(-90,-90),-90)
    
    # Reward probabilities (percent)
    prob_cor <- c(rep(75,7),rep(25,6),rep(75,5),rep(25,4),rep(25,3),rep(75,2),25)
    prob_incor <- c(c(25,75,25,25,75,25,75),c(75,25,25,75,25,75),c(25,25,75,25,75),c(25,75,25,75),c(75,25,75),c(25,75),75)
  
    # Task Label
    TaskName<-"Preference"
  }
  
  # -------------------------------------------------------
  # Assemble results into a data frame and compute derived variables
  # -------------------------------------------------------
  generated.outcomes <- 
    data.frame(symID_cor, symID_incor,
               rew_cor, rew_incor,
               prob_cor, prob_incor,
               TaskName,
               expType) %>% 
    rowwise() %>% 
    mutate(outcome_cor = rew_cor*rbinom(1,1,prob_cor/100),
           outcome_incor = rew_incor*rbinom(1,1,prob_incor/100)) %>% 
    ungroup() %>% 
    mutate(across(c(outcome_cor,outcome_incor),~ifelse(TaskName=="Preference",NA,.x))) %>% 
    mutate(ChoiceType = paste0(c(symID_cor),c(symID_incor)),
           vals_on = 0,
           ev_cor  = rew_cor*prob_cor/100,
           ev_incor  = rew_incor*prob_incor/100)
  
  # Randomize trial order
  generated.outcomes.interleaved <- 
    generated.outcomes[sample(nrow(generated.outcomes)),] %>% 
    mutate(trial = row_number()) %>% 
    arrange(trial) %>% 
    group_by(ChoiceType) %>% 
    mutate(trial.cond = seq_along(trial)) %>% 
    arrange(trial) 
  
  return(generated.outcomes.interleaved)
}

# =========================================================
# Simulate Participants' Responses
# =========================================================
# Generates the task structure (symbols, outcomes etc.)

# ----------Inputs----------------
#   pars        : transposed data.frame with parameter values (i.e. par names in rows)
#   pars_names  : vector with parameter names (in the right order)
#   set-Up      : list with settings for the model function (see -> f_model_Relative.R)
#   data.part*  : data.frame with trial data (--> if supplied, no need to generate one)
#   expType*    : if no data.frame is supplied, specify which experiment should be generated

#  *must supply one or the other.

# ----------Outputs---------------
#   results: 
#     --> data.frame with all relevant trial-level info + participant responses
# =========================================================

sim_RT_Relative <- function(pars,pars_names,model, setUp, data.part=NULL,expType=NA){
  
  
  # Generate unique participant ID for the simulation
  partID <- sample(seq(10000000,99999999),1)
  
  # -------------------------------------------------------
  # Set-up Parameters
  # -------------------------------------------------------

  # Convert parameter vector to named list
  p<-as.data.frame(pars) %>% t() %>% as.vector()
  names(p)<-pars_names
  
  # Two learning rates or one?
  if(setUp["lrType"]=="single"){
    p[["lr_r"]] = p[["lr_a"]]
  }
  
  # -------------------------------------------------------
  # Load Data + Preprocess Data
  # -------------------------------------------------------
  if(!is.null(data.part)){
    data<-data.part }
  else{
    data<-
      bind_rows(generate.outcomes("Learning",expType),
                generate.outcomes("Preference",expType))  }
  
  data<-
    data %>% 
    group_by(ChoiceType,TaskName) %>% 
    mutate(minChT = ifelse(is.na(outcome_cor),-1,min(outcome_cor,outcome_incor,na.rm=TRUE)),
           maxChT = ifelse(is.na(outcome_cor),1,max(outcome_cor,outcome_incor,na.rm=TRUE))) %>% 
    ungroup() %>% 
    mutate(outcome_cor.a = outcome_cor/90,
           outcome_incor.a  = outcome_incor/90,
           outcome_cor.rel = sign(outcome_cor-outcome_incor),
           outcome_incor.rel = sign(outcome_incor-outcome_cor))%>% 
    arrange(TaskName, trial)

  
  # -------------------------------------------------------
  # Extract trial-level vectors
  # -------------------------------------------------------
  expType <- unique(data$expType)
  TaskName <- data$TaskName
  
  symID_cor <- data$symID_cor
  symID_incor <- data$symID_incor
  
  outcome_cor.a <- data$outcome_cor.a
  outcome_incor.a <- data$outcome_incor.a

  vals_on<-data$vals_on
  
  # -------------------------------------------------------
  # Load Settings
  # -------------------------------------------------------
  
  # Choose relative coding method
  if(setUp["relative"]=="rel"){
    outcome_cor.r <- data$outcome_cor.rel
    outcome_incor.r <- data$outcome_incor.rel
  }
  
  # ------------------------------------------------
  # Initialize Q-values (0)
  # ------------------------------------------------
  QA<- rep(0,n_distinct(c(symID_cor,symID_incor)))
  QR <- rep(0,n_distinct(c(symID_cor,symID_incor)))
  
  # Pre-define vectors for simulation output
  choice.prob <- rep(NA,nrow(data))
  choice      <- rep(NA,nrow(data))
  
  QA.cor      <- rep(NA,nrow(data))
  QA.incor    <- rep(NA,nrow(data))
  QR.cor      <- rep(NA,nrow(data))
  QR.incor    <- rep(NA,nrow(data))
  
  QA.mat      <- matrix(nrow=nrow(data),ncol=length(QA))
  QR.mat      <- matrix(nrow=nrow(data),ncol=length(QR))
  
  # ---------------------------------------------------------
  # Trial Loop
  # ---------------------------------------------------------
  for(i in 1:nrow(data)){
    
    # ---Calculate Weighted Sum of Absolute And Relative Q-values ---
      M_cor <- p[["beta"]]*(QA[symID_cor[i]]*(1-p[["w"]]) +  p[["w"]]*QR[symID_cor[i]]) 
      M_incor  <- p[["beta"]]*(QA[symID_incor[i]]*(1-p[["w"]])   +  p[["w"]]*QR[symID_incor[i]])  
    
    # --- Calculate Softmax Probability ---
    choice.prob[i] <- logistic_simple(V_ch=M_cor, V_u =M_incor)
   
    # --- Determine Action Taken ---
    # (1 = correct option chosen, 0 = incorrect)
    choice[i] <- rbinom(1,1,choice.prob[i])
    
    if(is.na(choice[i])){
      print("problem")
    }
    
    # --- Value Update:  Learning Task Only ---
    if(TaskName[i]=="LearningTask"){
      
      # Values for current trial 
      #   - if correct option was selected, put cor first then incor 
      #   - if incorrect choice was selected do it the other way
      ids <- c(symID_cor[i],symID_incor[i])
      outcomes.a <- c(outcome_cor.a[i],outcome_incor.a[i])
      outcomes.r <- c(outcome_cor.r[i],outcome_incor.r[i])
      
      #Prediction Error Calculation
      delta.a <- outcomes.a - QA[ids]
      delta.r <- outcomes.r - QR[ids]
      
      #Q-value update
      QA[ids] <-   QA[ids] + p[["lr_a"]] * delta.a
      QR[ids] <-   QR[ids] + p[["lr_r"]] * delta.r 
      
    }
    
    # --- Save Updated Values ---

    QA.cor[i]   <- QA[symID_cor[i]]
    QA.incor[i] <- QA[symID_incor[i]]
    
    QR.cor[i]   <- QR[symID_cor[i]]
    QR.incor[i] <- QR[symID_incor[i]]

  }
  
  
  # ---------------------------------------------------------
  # Compile simulation results
  # ---------------------------------------------------------

  cols_to_keep <- c("outcome_cor", "outcome_incor", "ChoiceType","trial.cond",
                    "rew_cor", "rew_incor", "prob_cor", "prob_incor",
                    "symID_cor", "symID_incor", "TaskName","expType","trial")
  
  p2 <- rbind(p)
  
  results <- 
    data.frame(partID,
               partID.or = NA) %>% 
    bind_cols(.,data %>% select(all_of(cols_to_keep))) %>% 
    mutate(resp.cor=choice,
           choice_prob = choice.prob,
           QA_cor = QA.cor,
           QA_incor = QA.incor,
           QR_cor = QR.cor,
           QR_incor = QR.incor) %>% 
    mutate(model = model) %>% 
    mutate(outcome_ch = ifelse(resp.cor==1,outcome_cor,outcome_incor),
           outcome_u= ifelse(resp.cor==0,outcome_cor,outcome_incor),
           symID_ch = ifelse(resp.cor==1,symID_cor,symID_incor),
           symID_u= ifelse(resp.cor==0,symID_cor,symID_incor)) %>% 
    bind_cols(.,p2)
  
  
  if(is.data.frame(data.part)){
    results$partID.or<-data$partID
  }
  
  return(results)
}
# =========================================================


# =========================================================
#  CONVERT REAL DATA INTO SIMULATION FORMAT
# =========================================================
# Converts real experimental data into a
# format consistent with simulated data

convert_realData <- function(data){
  
  converted.data<-
    data %>% 
    ungroup() %>% 
    mutate(outcome_cor=ifelse(resp.cor==1,outcome_ch,outcome_u),
           outcome_incor=ifelse(resp.cor==0,outcome_ch,outcome_u),
           rew_cor=ifelse(resp.cor==1,rew_ch,rew_u),
           rew_incor=ifelse(resp.cor==0,rew_ch,rew_u),
           prob_cor=ifelse(resp.cor==1,prob_ch,prob_u),
           prob_incor=ifelse(resp.cor==0,prob_ch,prob_u),
           symID_cor =ifelse(resp.cor==1,symID_ch,symID_u), 
           symID_incor= ifelse(resp.cor==0,symID_ch,symID_u)) %>% 
    rename(trial.cond=trial.ChT)
}

# =========================================================
# LOGISTIC FUNCTIONS
# =========================================================
logistic_simple <- function(V_ch, V_u) {
  prob <- 1 / (1 + exp(V_u - V_ch))
}

