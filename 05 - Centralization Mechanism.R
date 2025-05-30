### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT


if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

### DOES GINI CENTRALIZATION increase EFFECT OF PHI?

library(lme4)

aggreg %>% 
  subset(communication=="Discussion") %>%
  glmer(improve ~ prop_toward*gini_talkativeness + (1|dataset), family="binomial", data=.) %>%
  summary

### HOW CENTRALIZED WAS IT?
mean(aggreg$gini_talkativeness, na.rm=T) # roughly equivalent to UK income
median(aggreg$gini_talkativeness, na.rm=T) # same as mean
range(aggreg$gini_talkativeness, na.rm=T) # upper is USA income
hist(aggreg$gini_talkativeness) # fairly normal
quantile(aggreg$gini_talkativeness, na.rm=T)


