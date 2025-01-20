if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(lmerTest)

###########



repd = subset(aggreg, analysis=="replication")
reanal = subset(aggreg, analysis=="reanalysis")


### CONTROLLING FOR VARIANCE - MAIN EFFECT

glm(improve ~ dataset + var + prop_toward, family="binomial", data=aggreg) %>% 
  summary

### CONTROLLING FOR INDIVIDUAL ERROR - MAIN EFFECT

glm(improve ~ dataset + ind_err + prop_toward, family="binomial", data=aggreg) %>% 
  summary


### CONTROLLING WITH INTERACTION 

glm(improve ~ dataset + var + prop_toward*communication, family="binomial", data=aggreg) %>% 
  summary
