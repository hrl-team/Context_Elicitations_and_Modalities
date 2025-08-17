#=========================================================
# Functions to load data from the original csv files
#=========================================================

# Series of functions to clean and transform the raw csv data into an R data frame

#=========================================================
# getData_RT:
#=========================================================
# Uses the pattern defined in taskList to find all folders in path specified in the argument path
# Then calls the functions below to extract data, exclude trials/participants and add useful columns for later analysis
# The context argument refers to the path to the file with Choice Set definitions for each combinations of options
# Common format for the taskList = "RT_(FF..|FF..S|FF..U|CF..U|RF..U|FH..U|FM..U)B?$" 

#=========================================================
#loadFiles_RT:
#=========================================================
# These folders are then searched for the files with relevant strings:
# --- Learning, Preference, Valuation (Recollection Task), SMSQ(Questionnaire),
# --- General(list of all who finished the whole experiment)
# --- Prolific (data provided by Prolific)

#=========================================================
# exclusion_RT:
#=========================================================
# Return IDs of anyone who 
# --- finished the Learning Task, but restarted partway through follow-up tasks
# --- had any missing trials
# --- had accuracy below 0.5
# --- also return the trials that need to be excluded even though the participant is kept in the sample 
# --- (e.g. those that restarted partway through the learning task)

#=========================================================
# AddColumns_RT:
#=========================================================
# --- add columns that will be useful for later analysis
# --- Turn expID into a label (E1-E7)
# --- Add 1/0 for correct/incorrect responses (resp.cor), + ev_cor, ev_incor
# --- unify sym_ID, across experiments
# --- unify notation across different versions of value recollection task


#=========================================================
# Functions
#=========================================================
# Get Data ####
getData_RT<- function(taskList,path,context){
  
  #get paths to all folders that specify the pattern specified in taskList
  exp.folders <- list.files (path = path,pattern = taskList,full.names = TRUE)
  
  #load files
  df <-mapply(loadFiles_RT,path.exp=exp.folders)
  
  # extract the df for each task from the experiment sublists
  # and put each of them into a single df
  all.RT <- 
    list(
      learning = bind_rows(map(df, "learning")),
      preference = bind_rows(map(df, "preference")),
      valuation = bind_rows(map(df, "valuation")),
      demo = bind_rows(map(df, "demo")),
      SMSQ = bind_rows(map(df, "SMSQ")),
      prolific = bind_rows(map(df, "prolific"))
    )
  
  #Exclude participants who restarted the task partway, or had some missing trials or low accuracy
  cleaned.RT<-exclusion_RT(all.RT[c(1:5)])
  cleaned.RT$data$prolific <- all.RT$prolific
  
  # Add columns that will be used for later analysis
  data<- addColumns_RT(cleaned.RT$data,context=context)
  
  data$IDs <-list(r.weirdTrialN = cleaned.RT$excluded$partID,
                  r.lowAccuracy = cleaned.RT$excluded$acc,
                  final = cleaned.RT$kept$partID,
                  completed  = cleaned.RT$original$partID)
  
  return(data)
}

# Load Files ####
loadFiles_RT<- function(path.exp){ 
  
  taskVersion <- basename(path.exp)
  
  # demographics
  df.general <- read.csv(list.files(path =path.exp,pattern = "_General.csv",full.names = TRUE))
  
  df.prolific <- 
    read.csv(list.files(path =path.exp,pattern = "_Prolific.csv",full.names = TRUE)) %>% 
    rename(partID = participant_id)
    
  df.demo<-
    merge(df.general,subset(df.prolific,status %in% c("AWAITING REVIEW","APPROVED","TIMED-OUT")),all.x = TRUE) %>% 
    rename(expID = ExpID) %>% 
    filter(totalTime!=0)
  
  df.prolific$expID<- taskVersion
  
  if(taskVersion == "RT_FFP1B"){df.demo$ExpID<-"RT_FFP1B"}
  
  part.finished <- unique(df.demo$partID)
  
  list.data <- list(demo=df.demo)
  list.data <- c(list.data,list(prolific=df.prolific))
  
  # Learning task 
  if (substr(taskVersion,7,7)==1){
    
    df.learning<- 
      read.csv(list.files(path =path.exp,pattern = "_Learning.csv",full.names = TRUE))%>% 
      filter(partID%in%part.finished) %>% 
      filter(!partID=="")

    if(taskVersion == "RT_FFP1B"){df.demo$ExpID <- "RT_FFP1B"}
    if (substr(taskVersion,4,4)=="R"){df.learning$TaskName <- "LearningTask"}
      
    list.data <- c(list.data,list(learning=df.learning))
  }
  
  # Preference task
  df.preference<- 
    read.csv(list.files(path =path.exp,pattern = "_Preference.csv",full.names = TRUE))%>% 
    filter(TaskName=="Preference") %>%
    filter(partID%in%part.finished)%>% 
    filter(!partID=="")
  
  if(taskVersion == "RT_FFP1B"){df.demo$ExpID <- "RT_FFP1B"}
  
  list.data <- c(list.data,list(preference=df.preference))
  
  # Valuation task
  df.valuation<- 
    read.csv(list.files(path =path.exp,pattern = "_Valuation.*.\\csv",full.names = TRUE))%>% 
    filter(partID%in%part.finished) %>% 
    mutate(TaskVersion = TaskName,
           TaskName = "Valuation")%>% 
    filter(!partID=="")
  
  if (substr(taskVersion,8,8)=="S"){df.valuation$TaskVersion <- "ValuationsSlider"}
  if(taskVersion == "RT_FFP1B"){df.demo$ExpID <- "RT_FFP1B"}
  
  list.data <- c(list.data,list(valuation=df.valuation))
  
  #SMSQ questionnaire 
  if (substr(taskVersion,7,7)==1){
    df.SMSQ<- 
      read.csv(list.files(path =path.exp,pattern = "_SMSQ.csv",full.names = TRUE)) %>% 
      filter(partID%in%part.finished)%>% 
      filter(!partID=="")%>% 
      rename(expID = ExpID)
    
    list.data <- c(list.data,list(SMSQ=df.SMSQ))
  }
  
  return(list.data)
}

# Exclude ####
exclusion_RT<- function(all.list){
  # record all partIDs that completed the task (and did not withdraw consent)
  original <- 
    all.list$learning %>% 
    distinct(partID)
  
  # take all data and check if some participants did any of the tasks more then once:
  ## check and remove any duplicates in the database (same results inserted twice)
  ## Number the tasks in order they were taken (e.g. 1_Learning, 2_Preference, 3_Valuation), PER DAY (EXPID)
  ## Issues examples: 0_Learning (1st trial missing or has later DBTIME than 2nd trial), 3_Learning (Experiment restarted partway)
  ## In case of 0_learning, merge it with 1_Learning
  data.repeats<-
    bind_rows(all.list$learning,all.list$preference,all.list$valuation) %>% 
    #filter(substr(expID,7,7)==1) %>% 
    mutate(d=duplicated(select(.,-DBTIME))) %>%  
    filter(d==FALSE) %>% 
    group_by(partID,expID) %>% 
    arrange(DBTIME) %>% 
    mutate(totalTrial = seq(DBTIME)) %>% 
    group_by(partID,expID) %>%
    arrange(DBTIME) %>% 
    mutate(taskStartNum = ifelse(trial ==0, 1,0)) %>% 
    mutate(round = paste0(cumsum(taskStartNum),"_",TaskName)) %>% 
    mutate(round = ifelse(round == "0_LearningTask","1_LearningTask",round))
    
  
  # Calculate when was the first learning and follow-up task taken 
  ## Count the number of observations by PartID and Task Repeat and expID (To distinguish D1 an D2)
  ## Rename Preference Task + Valuation task to Follow-up (TaskName2)
  ## Then regroup by TaskName2 and PartID, and find the lowest number for each learning task and Follow up.
  ## Ideally, the lowest learning task should be 1 for learning, and 2 for follow up
  decision.criteria<-
    data.repeats %>% 
    group_by(partID,expID,round) %>% 
    summarise(n=n()) %>% 
    mutate(round_num = parse_number(round),
           taskName = substr(round, nchar(round_num)+2,nchar(round)),
           taskName2=ifelse(taskName=="LearningTask",taskName,"FollowUp")) 
   
  mins <- 
    decision.criteria %>% 
    group_by(partID,expID,taskName2) %>% 
    summarise(minV = min(round_num)) %>% 
    pivot_wider(names_from = taskName2, values_from = minV, names_prefix= "min_")
  
  # Mark any "extra" trials that will need to be excluded from the analysis, i.e:
  # Any trials that were repeats:
  # -- If participant started learning task again after doing at least one trial of the follow-up task
  # -- If participant started any follow up task after completing after doing at least one trial of 2 follow-up tasks
  # Any trials that were False starts:
  # -- If participant restarted the learning task before finishing it, exclude all learning trials from the first, incomplete round
  problematic.trials<-
    merge(decision.criteria,mins) %>% 
    mutate(repeat_round = case_when(taskName == "LearningTask"&round_num > min_FollowUp~"delete",
                                    substr(expID,7,7)==1&taskName != "LearningTask"&round_num > min_FollowUp+1~"delete",
                                    substr(expID,7,7)==2&taskName != "LearningTask"&round_num > 2~"delete",
                                    TRUE~"_"),
           false_start = ifelse(taskName == "LearningTask"&round_num < min_FollowUp-1,"delete","_"),
           round_exl = ifelse(repeat_round == "delete" | false_start == "delete", "delete", "_")) 
  
  # Mark any participants that will need to be excluded from the analysis
  # First exclude all trials marked for rejection from the procedure above
  # Then look for problematic participants: 
  # --Those who fully completed the learning task and then restarted (as they might have read the instruction for the follow ups)
  # --Those who did not finish all three tasks uninterrupted (i.e. those that restarted partway through the follow-up tasks)
  # --Those that had any missing trials (i.e. less than the mandatory 112 for LT, 28 for Preference, 24 for Valuation)
   problematic.partID<-
    problematic.trials %>% 
    filter(round_exl=="_") %>% 
    group_by(partID,expID) %>% 
    mutate(nTasks = n()) %>% 
    mutate(excl.part.FS = ifelse(false_start=="delete"&n==112,"delete","_"),
           excl.part.nTasks = case_when(substr(expID,7,7)==1&nTasks!=3~"delete",
                                        substr(expID,7,7)==2&nTasks!=2~"delete",
                                        TRUE~"_"),
           excl.part.miss = case_when(taskName=="LearningTask"&n<112~"delete",
                                      taskName=="Preference"&n<28~"delete",
                                      taskName=="Valuation"&n<24~"delete",
                                      TRUE~"_"),
           excl.part.final = ifelse(excl.part.FS == "delete" | excl.part.miss == "delete"|excl.part.nTasks== "delete","delete", "_")) 
   
  # List of participants/trials to exclude
   partID.delete<-
     unique(subset(problematic.partID,excl.part.final=="delete")$partID)
   
   trials.delete<-
     merge(data.repeats,problematic.trials) %>% 
     filter(!partID%in%partID.delete) %>% 
     select(expID,partID,DBTIME,trial,taskName,round,round_exl) %>% 
     rename(TaskName=taskName)
   
   # Mark any participants with low accuracy (<0.5) in the learning task
    problematic.accuracy<-
     all.list$learning %>% 
     bind_rows(tibble(vals_on=numeric())) %>% 
     filter(!partID%in%partID.delete) %>% 
     merge(.,trials.delete) %>% 
     filter(round_exl=="_") %>% 
     mutate(resp.cor = rew_ch*prob_ch>rew_u*prob_u) %>% 
     filter(is.na(vals_on)|vals_on!=1) %>% 
     group_by(partID) %>% 
     summarise(accuracy = mean(resp.cor)) %>% 
     mutate(excl.part.acc = ifelse(accuracy<0.5,"delete","_")) 
  
    acc.delete<-
      unique(subset(problematic.accuracy,excl.part.acc=="delete")$partID)
    
### EXCLUDE MARKED TRIALS/PARTICIPANTS + SUMMARIZE THOSE THAT WERE EXCLUDED
    
  toExclude <- list(partID=partID.delete,
                    trials=trials.delete,
                    acc=acc.delete)
    
  data.list<-lapply(all.list,
                    function(x,y) x<-
                      x %>% 
                      filter(!partID%in%y$partID) %>%
                      filter(!partID%in%y$acc),y=toExclude)
  
  data.list[c(1:3)]<-
    lapply(data.list[c(1:3)],
           function(x,y) x<-
             x %>% 
             merge(.,toExclude$trials) %>% 
             filter(round_exl=="_"),
           y=toExclude)
  
  output <- list(data = data.list,
                 excluded = toExclude, 
                 kept = data.list$learning %>% distinct(partID),
                 original = original)
  
  return(output)
}


# Add Columns ####
addColumns_RT<-function (all.list, context){
  
  # Remove participants that were to be excluded due to low accuracy and/or missing trials/restarting partway
  # Add experiment info columns, useful for all task Types
  data.list<-map(all.list, ~ .x %>%
                              mutate(Day = ifelse(substr(expID,7,7)==1,1,2),
                                     partID = as.factor(partID),
                                     Run =  ifelse(substr(expID,str_length(expID),str_length(expID)) == "B", "B","A"),
                                     FirstTask = ifelse(substr(expID,6,6)=="P","Preference","Valuation"),
                                     expType = paste0(substr(expID,4,5),"_",ifelse(substr(expID,8,8)%in%c("U","S"),substr(expID,8,8),"O")),
                                     expType = case_when(expType == "FF_U" ~ "E3",
                                                        expType == "FF_O" ~ "E1",
                                                        expType == "FF_S" ~ "E2",
                                                        expType == "CF_U" ~ "E6",
                                                        expType == "RF_U" ~ "E7",
                                                        expType == "FM_U" ~ "E5",
                                                        expType == "FH_U" ~ "E4"),
                                     expType = relevel(factor(expType),"E3"),
                                     Day.f = as.factor(Day)))

  
  # Add columns useful for both the learning and preference task
  data.list[c(1:2)]<-
    map(data.list[c(1:2)], ~ .x %>%
                           mutate(ev_ch = prob_ch*rew_ch/100, 
                                  ev_u = prob_u*rew_u/100,
                                  across(c(ev_ch,ev_u),~case_when(.x == -67.5 ~ 1,
                                                                        .x == -22.5 ~ 2,
                                                                        .x == -7.5 ~ 3,
                                                                        .x == -2.5 ~ 4,
                                                                        .x == 2.5 ~ 5,
                                                                        .x== 7.5 ~ 6,
                                                                        .x== 22.5 ~ 7,
                                                                        .x == 67.5~8),
                                         .names = "{gsub('ev', 'symID', {col}, fixed = TRUE)}"),
                                  resp.cor = ifelse(ev_ch>ev_u,1,0),
                                  ev_cor = ifelse(ev_ch>ev_u,ev_ch,ev_u),
                                  ev_incor = ifelse(ev_ch>ev_u,ev_u,ev_ch),
                                  ChoiceType.f = as.factor(10*ifelse(resp.cor==1,symID_ch,symID_u)+1*ifelse(resp.cor==0,symID_ch,symID_u)),
                                  respRT.s = respRT/1000) %>%
                           group_by(partID,ChoiceType.f,expID) %>% 
                           mutate(trial.ChT = seq_along(trial)) %>% 
                           merge(.,context,by=c("ev_cor","ev_incor")) %>% 
                           mutate(relative = ifelse(expType=="E6",ChoiceByPrev_C,ChoiceByPrev_F)))
  
  # Some more columns for the learning task:
  data.list$learning<-
    data.list$learning %>%
    mutate(magnitude = as.factor(ifelse(abs(rew_ch)==90,"high","low")),
           valence = as.factor(ifelse(rew_ch >0, "positive","negative")),
           frequency = as.factor(ifelse(prob_ch >0, "high","low"))) 
  
  # Some more columns for the preference task:
  data.list$preference<-
    data.list$preference %>%
    mutate(vals_on = NA)
  

  
  # SORT OUT VALUATION TASK
  
  data.list$valuation<- prepareValuation_RT(data.list$valuation)
  
  return(data.list)
  
} 
prepareValuation_RT<- function(all.valuation)   {
  
  if(all.valuation %>% filter(TaskVersion=="ValuationsSlider") %>% nrow() > 1){
    data.valuation.slider<-
      all.valuation %>%
      filter(TaskVersion=="ValuationsSlider") %>%
      mutate(sym_prob = prob_sym,
             sym_reward = reward_sym,
             sym_ev = ev_symbol) %>%
      mutate(resp_int = resp_ev) %>%
      mutate(resp_cor_i = case_when(sign(resp_ev) != sign(sym_ev) ~ 0,
                                    sym_prob == 75 & abs(resp_ev)>=abs(sym_reward*0.5) & abs(resp_ev)<=abs(sym_reward) ~ 1,
                                    sym_prob == 25 & abs(resp_ev)>=0 & abs(resp_ev)<=abs(sym_reward)*0.5  ~ 1,
                                    TRUE ~ 0))
  }else{
    data.valuation.slider<-
      all.valuation %>%
      filter(TaskVersion=="ValuationsSlider")
  }

  if(all.valuation %>% filter(TaskVersion=="Valuations") %>% nrow() > 1){
  data.valuation.outcome<-
    all.valuation %>%
    filter(TaskVersion=="Valuations") %>%
    mutate(sym_prob = prob_sym,
           sym_reward = reward_sym) %>% 
    mutate(sym_ev = sym_prob*sym_reward/100,
           resp_int = reward_ch,
           resp_cor_i = resp_cor)
  }else{
    data.valuation.outcome<-
      all.valuation %>%
      filter(TaskVersion=="Valuation")
  }
  
  
  if(all.valuation %>% filter(TaskVersion=="ValuationsUnited") %>% nrow() > 1){
  data.valuation.united<- 
    all.valuation%>% 
    filter(TaskVersion=="ValuationsUnited") %>% 
    mutate(resp_int = resp_ev) %>%  
    mutate(resp_probCor_i = ifelse(sign(resp_prob-50)==sign(sym_prob-50),1,0),
           resp_cor_i = resp_rewCor&resp_probCor_i)
  }else{
    data.valuation.united<- 
      all.valuation%>% 
      filter(TaskVersion=="ValuationsUnited")
  }
  
  
  data.valuation <- 
    bind_rows(data.valuation.united,data.valuation.outcome,data.valuation.slider, .id = NULL) %>% 
    mutate(resp_init = ifelse(expType!="E2",init_prob==resp_prob,init_ev==resp_ev),
           vals_on= NA,
           symID = case_when(sym_ev == -67.5 ~ 1,
                              sym_ev == -22.5 ~ 2,
                              sym_ev == -7.5 ~ 3,
                              sym_ev == -2.5 ~ 4,
                              sym_ev == 2.5 ~ 5,
                              sym_ev== 7.5 ~ 6,
                              sym_ev == 22.5 ~ 7,
                              sym_ev == 67.5~8,
                              TRUE~ 99), 
           symID.f = factor(sym_ev,
                            levels=c(-67.5,-22.5,-7.5,-2.5,2.5,7.5,22.5,67.5),
                            labels=c("A","B","C","D","E","F","G","H")),
           rank_obj = case_when(expType=="E2"&resp_int<=-45~0,
                                expType=="E2"&resp_int>-45&resp_int<=-10~1,
                                expType=="E2"&resp_int<=-5&resp_int>-10~2,
                                expType=="E2"&resp_int>-5&resp_int<=0~3,
                                expType=="E2"&resp_int<=5&resp_int>0~4,
                                expType=="E2"&resp_int>5&resp_int<=10~5,
                                expType=="E2"&resp_int<=45&resp_int>10~6,
                                expType=="E2"&resp_int>45~7,
                                expType=="E1"&resp_int==-90~0,
                                expType=="E1"&resp_int==-10~1,
                                expType=="E1"&resp_int==0~2,
                                expType=="E1"&resp_int==10~3,
                                expType=="E1"&resp_int==90~4,
                                resp_rew==-90&resp_prob>50~0,
                                resp_rew==-90&resp_prob<=50~1,
                                resp_rew==-10&resp_prob>50~2,
                                resp_rew==-10&resp_prob<=50~3,
                                resp_rew==10&resp_prob<=50~4,
                                resp_rew==10&resp_prob>50~5,
                                resp_rew==90&resp_prob<=50~6,
                                resp_rew==90&resp_prob>50~7),
           rank_obj_1 =rank_obj +1,
           val_abs = (symID-1),
           val_rel = case_when(expType%in%c("E1","E2","E3","E4","E5") & symID.f%in%c("B","D","F","H")~"good",
                               expType%in%c("E1","E2","E3","E4","E5") & symID.f%in%c("A","C","E","G")~"bad",
                               expType=="E6"& symID.f%in%c("C","D","G","H")~"good",
                               expType=="E6"& symID.f%in%c("A","B","E","F")~"bad",
                               TRUE~"undefined")) %>% 
    rename(resp.cor = resp_cor_i) %>% 
    select(-c(symbol_ID)) 
  
  
  return(data.valuation)
  
}
