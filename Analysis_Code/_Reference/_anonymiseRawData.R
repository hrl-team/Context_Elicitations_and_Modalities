############################################################
 
# ANONYMISE RAW DATA

############################################################
# Script used to anonymise the original partID
# Converts the original partIDs into a combination of a letter P
# and a number based on their alphabetical order

# ============================================================
# Generate the new IDs
# ============================================================

newIDs <-
  data$demo %>%
  mutate(partID_new = paste0("P",as.numeric(as.factor(partID)))) %>% 
  mutate(partID_new = as.factor(partID)) %>% 
  distinct(partID,partID_new)

# Add them to the data list
data2<-list()
data2<-lapply(data[-c(6,7)], 
              function(x) merge(x, newIDs, by = 'partID'))

# ============================================================
# Test whether the names are the same across all lists  
# ============================================================
 
test<-
  bind_rows(data2) %>% 
  distinct(partID,TaskName,partID_new) %>% 
  pivot_wider(names_from = TaskName,values_from = partID_new)

# ============================================================
# Delete the original partID 
# ============================================================

data3<-lapply(data2,
                  function(x) x<-
                    x %>% 
                    mutate(partID=partID_new,
                           partID_new = NULL))

data3$demo$prolificID <- NULL

# ============================================================
# Save Anonymised Data + the original vs new ID pairing list
# ============================================================
saveRDS(data3,"data_RT_Anon_25.Rds")

# Save the pairings
saveRDS(newIDs,"ID_keys.Rds")
