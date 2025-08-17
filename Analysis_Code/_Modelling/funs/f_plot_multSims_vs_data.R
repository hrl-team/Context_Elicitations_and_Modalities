###########################################################

# Compare simulations to real data

###########################################################
# Compares date simulated from multiple models vs. real data 
# Created to quickly plot falsification (with a one-liner)
# Not as universal as it could be

# ==========================================================
# ----------Inputs------------------------------------------
#   sim       : data.frame (or list) with simulations
#   data      : empirical (real) dataset
#   myPalette : vector of colors for plotting

# ----------Outputs-----------------------------------------
#   Outcome: List of figures

# ----------Dependencies------------------------------------
# patchwork, dplyr, ggplot2, ggh4x
# ==========================================================


plot_multSims_vs_data <- function(sim,data,myPalette){
   require(patchwork)
 
  # if a list was supplied -> turn in into a data frame
  if(is.list(sim)==TRUE){
    sim <- bind_rows(sim)
  }
  
  # -----------------------------------------------------------
  # FIG 1: Accuracy – Learning Task (Overall)
  # -----------------------------------------------------------
  # process real data 
  acc <-
    data %>%
    mutate(model = c("real data")) %>%
    filter(TaskName=="LearningTask")%>%
    group_by(partID,ChoiceType.f,expType, TaskName,model) %>% 
    summarise(accuracy=mean(resp.cor,na.rm=TRUE)) %>% 
    mutate(ChoiceType=ChoiceType.f )%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # process simulated data
  sim.acc<-
    sim %>% 
    mutate(partID=as.character(partID.or)) %>% 
    filter(TaskName=="LearningTask")%>%
    group_by(partID,ChoiceType,expType, TaskName,model) %>% 
    summarise(accuracy=mean(resp.cor))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
    
  
  # plot
  f1<- ggplot(aes(x=ChoiceType,y=accuracy),data=acc)+
    stat_summary(fun.data = "mean_se", geom = "bar",fun.args = list(mult = 1),position=position_dodge(width=0.5),fill="gray80")+
    stat_summary(fun.data = "mean_se", geom = "errorbar",fun.args = list(mult = 1),size=1,width=0.3, position=position_dodge(width=0.5))+
    stat_summary(fun.data = "mean_se", geom = "point",fun.args = list(mult = 1),position=position_dodge(width=0.5),data=sim.acc,aes(color=model),size=2)+
    ylim(0,1)+
    ggh4x::facet_grid2(TaskName~expType, scales="free_x", independent = "x")+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0))+
    scale_color_manual(values=myPalette)
  
  # -----------------------------------------------------------
  # FIG 2: Accuracy – Learning Task (Per Trial)
  # -----------------------------------------------------------
  # process real data 
  acc.T <-
    data %>%
    mutate(model = ("real data")) %>% 
    mutate(model2 = model) %>% 
    group_by(partID,trial.cond,ChoiceType.f,expType, TaskName,model2) %>% 
    summarise(accuracy=mean(resp.cor))%>% 
    mutate(ChoiceType=ChoiceType.f)%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # process simulated data 
  sim.acc.T<-
    sim %>% 
    mutate(partID=as.character(partID.or)) %>% 
    group_by(partID, trial.cond,ChoiceType,expType, TaskName,model) %>% 
    summarise(accuracy=mean(resp.cor))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # plot
  f2<- ggplot(aes(x=trial.cond,y=accuracy),
              data=sim.acc.T %>% filter(TaskName=="LearningTask") %>% filter(expType!="E7"))+
    #stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2,na.rm=TRUE,aes(color=model))+
    facet_grid(model~ChoiceType)+
    stat_summary(fun.data = mean_se, geom = "line",linewidth=0.8, alpha = 1,na.rm=TRUE, data= acc.T %>% filter(TaskName=="LearningTask"),color="gray50")+
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2,na.rm=TRUE, data= acc.T %>% filter(TaskName=="LearningTask"),color="gray50",alpha=0.5)+
    stat_summary(fun.data = mean_se, geom = "line",linewidth=0.8, alpha = 1,na.rm=TRUE,aes(color=model))+
    ylim(0,1)+
    theme_classic(base_size = 18)+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0))+
    scale_color_manual(values=myPalette)
  
  # -----------------------------------------------------------
  # FIG 3: Choice Rate - Learning Task + Preference Task
  # -----------------------------------------------------------
  # process real data 
  real.zigzag<-
    data%>% 
    mutate(model = c(".Real Data")) %>% 
    ungroup() %>% 
    select(partID,expType,symID_ch,symID_u,model,TaskName) %>% 
    pivot_longer(cols = c(symID_ch,symID_u),
                 names_to=c(".value","chosen"),names_sep = "([_])") %>% 
    mutate(was_chosen_numeric = ifelse(chosen == "ch", 1,0)) %>% 
    group_by(partID,expType,model,symID,TaskName) %>% 
    summarise(prop.chosen=mean(was_chosen_numeric))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # process simulated data 
  sim.zigzag<-
    sim%>% 
    ungroup() %>% 
    select(partID,expType,symID_ch,symID_u,model,TaskName) %>% 
    pivot_longer(cols = c(symID_ch,symID_u),
                 names_to=c(".value","chosen"),names_sep = "([_])") %>% 
    mutate(was_chosen_numeric = ifelse(chosen == "ch", 1,0)) %>%
    group_by(partID,expType,model,symID,TaskName) %>% 
    summarise(prop.chosen=mean(was_chosen_numeric))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7"))) 
  
  # plot - both tasks
  f3<- ggplot(aes(x=factor(symID,levels=c(1,2,3,4,5,6,7,8),labels=c("A","B","C","D","E","F","G","H")),
                   y=prop.chosen),data=real.zigzag)+
    stat_summary(fun.data = "mean_se", geom = "bar",fun.args = list(mult = 1),position=position_dodge(width=0.5),fill="gray80")+
    stat_summary(fun.data = "mean_se", geom = "errorbar",fun.args = list(mult = 1),size=1,width=0.3, position=position_dodge(width=0.5))+
    stat_summary(fun.data = "mean_se", geom = "point",fun.args = list(mult = 1),position=position_dodge(width=0.5),data=sim.zigzag,
                 aes(color=model),size=2.5)+
    ylim(0,1)+
    theme_classic(base_size = 18)+
    facet_grid(TaskName~expType,labeller = labeller(TaskName=c(LearningTask="Learning",
                                                               Preference="Preference"),
                                                    expType = c(E1="E1",
                                                                E2="E2",
                                                                E3="E3",
                                                                E4="E4",
                                                                E5="E5",
                                                                E6="E6",
                                                                E7="E7")))+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0))+
    scale_color_manual(values=myPalette)+
    xlab("") + ylab("Choice Rate")
  
  # plot - learning task only
  f3A<- ggplot(aes(x=factor(symID,levels=c(1,2,3,4,5,6,7,8),labels=c("A","B","C","D","E","F","G","H")),
                           y=prop.chosen),data=real.zigzag %>% filter(TaskName=="LearningTask"))+
    stat_summary(fun.data = "mean_se", geom = "bar",fun.args = list(mult = 1),position=position_dodge(width=0.5),fill="gray80")+
    stat_summary(fun.data = "mean_se", geom = "errorbar",fun.args = list(mult = 1),size=1,width=0.3, position=position_dodge(width=0.5))+
    stat_summary(fun.data = "mean_se", geom = "point",fun.args = list(mult = 1),position=position_dodge(width=0.5),data=sim.zigzag%>%filter(TaskName=="LearningTask"),
                 aes(color=model),size=2.5)+
    ylim(0,1)+
    theme_classic(base_size = 18)+
    facet_grid(TaskName~expType,labeller = labeller(TaskName=c(LearningTask="Learning",
                                                              Preference="Preference"),
                                                    
                                                    expType = c(E1="E1",
                                                                E2="E2",
                                                                E3="E3",
                                                                E4="E4",
                                                                E5="E5",
                                                                E6="E6",
                                                                E7="E7")))+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0))+
    scale_color_manual(values=myPalette)+
    xlab("") + ylab("Choice Rate")
  
  # plot - preference task only
  f3B<- ggplot(aes(x=factor(symID,levels=c(1,2,3,4,5,6,7,8),labels=c("A","B","C","D","E","F","G","H")),
                   y=prop.chosen),data=real.zigzag %>% filter(TaskName=="Preference"))+
    stat_summary(fun.data = "mean_se", geom = "bar",fun.args = list(mult = 1),position=position_dodge(width=0.5),fill="gray80")+
    stat_summary(fun.data = "mean_se", geom = "errorbar",fun.args = list(mult = 1),size=1,width=0.3, position=position_dodge(width=0.5))+
    stat_summary(fun.data = "mean_se", geom = "point",fun.args = list(mult = 1),position=position_dodge(width=0.5),data=sim.zigzag %>% filter(TaskName=="Preference"),
                 aes(color=model),size=2.5)+
    ylim(0,1)+
    theme_classic(base_size = 18)+
    facet_grid(TaskName~expType,labeller = labeller(TaskName=c(LearningTask="Learning",
                                                               Preference="Preference"),
                                                    
                                                    expType = c(E1="E1",
                                                                E2="E2",
                                                                E3="E3",
                                                                E4="E4",
                                                                E5="E5",
                                                                E6="E6",
                                                                E7="E7")))+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0))+
    scale_color_manual(values=myPalette)+
    xlab("") + ylab("Choice Rate")
  

  # -----------------------------------------------------------
  # FIG 4: Preference Task: Choice Set Accuracy
  # -----------------------------------------------------------
  
  # Define Choice Sets (from real data)
  context <-
    data %>% 
    ungroup() %>% 
    distinct(TaskName,ChoiceType.f,relative,expType) %>% 
    mutate(ChoiceType = ChoiceType.f)%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # process real data 
  acc2 <-
    data %>%
    filter(TaskName=="Preference") %>% 
    mutate(model = c(".RealData"),
           ChoiceType = as.factor(ChoiceType.f)) %>% 
    ungroup() %>% 
    group_by(partID,expType, TaskName,model,relative) %>% 
    summarise(accuracy=mean(resp.cor,na.rm=TRUE))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # process simulated data (Adds Choice Sets variable!)
  sim.acc2<-
    sim %>% 
    filter(TaskName=="Preference") %>% 
    merge(.,context,by=c("ChoiceType","TaskName","expType")) %>% 
    ungroup() %>% 
    mutate(partID=as.character(partID.or)) %>% 
    group_by(partID,expType, TaskName,model,relative) %>% 
    summarise(accuracy=mean(resp.cor,na.rm=TRUE))%>% 
    mutate(expType=factor(expType,levels=c("E1","E2","E3","E4","E5","E6","E7")))
  
  # plot 
  f4<- ggplot(aes(x=relative,y=accuracy),data=acc2)+
    stat_summary(fun.data = "mean_se", geom = "bar",fun.args = list(mult = 1),position=position_dodge(width=0.5),fill="gray80")+
    stat_summary(fun.data = "mean_se", geom = "errorbar",fun.args = list(mult = 1),size=1,width=0.3, position=position_dodge(width=0.5))+
    stat_summary(fun.data = "mean_se", geom = "point",fun.args = list(mult = 1),position=position_dodge(width=0.5),data=sim.acc2,aes(color=model),size=2.5)+
    ylim(0,1)+
    facet_grid(TaskName~expType)+
    theme_classic(base_size = 18)+
    scale_x_discrete(limits = c("Old", "New_1", "New_2", "New_3"),labels = c("Old","New1","New2","New3"))+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.position = "bottom")+
    scale_color_manual(values=myPalette)+
    xlab("") + ylab("EV-maximising choices")
 
  # -----------------------------------------------------------
  # Compile the plots into one big figure + list of individual plots
  # ----------------------------------------------------------- 
  plots <- f3A+plot_spacer()+f3B+plot_spacer()+f4 + patchwork::plot_layout(nrow = 5,heights=c(5,-1.5,5,-1.5,5),guides = "collect") & theme(legend.position = "bottom")
  
  outcome <- list(plots,f1,f2,f3,f4)
  
  return(outcome)
  

}
