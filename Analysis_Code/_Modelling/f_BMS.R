## Prep data for BMS - using AIC only ####
prep.BMS <- function(data,partID=FALSE,method){ 
  
  # Calculate AKaike Weights #
  #(sum2 there just to check that the weights are calculating properly - not exported)
  bms.data <-
    data %>%
    group_by(partID,model) %>%
    mutate(npar = round((AIC-2*value)/2),
           BIC2 = 2*value+npar*log(140)) %>% 
    mutate(type = ifelse(method=="AIC",AIC,BIC2)) %>% 
    distinct(partID,type,model) %>%
    group_by(partID) %>%
    mutate(min_mod = min(type),
           rel.LL = exp(-0.5*(type-min_mod)),
           sum.LL = sum(rel.LL),
           weight = rel.LL/sum.LL,
           sum2 = sum(weight)) %>%
    select(partID,model,weight) %>%
    pivot_wider(names_from = model,values_from = weight,names_prefix = "mod_")
  
  # by default do not return participants ID
  if(partID==FALSE){
    bms.data <-
      bms.data %>% 
      ungroup %>% 
      select(-c(partID))
  }
  
  return(bms.data)
}


do.bms<- function(data,partID=FALSE,method){
  names <- unique(data$model)
  
  bms<- 
    as.data.frame(bmsR::VB_bms(as.matrix(prep.BMS(data,partID=FALSE,method)))) %>% 
    mutate(model = names)
}