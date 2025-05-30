### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT

rm(list=ls());gc()
library(tidyverse, warn.conflicts = F, quietly = T)
library(DescTools, warn.conflicts = F, quietly = T)

source("00a_data_prep_REANALYSIS.R")
source("00b_data_prep_REPLICATION.R")


cols=c("pre_influence","post_influence","truth","task","trial"
       ,"communication","count_chat","analysis","dataset"
       , "soc_info") 

d = rbind(
    delphi_replication[,cols] %>% as.data.frame(stringsAsFactors=F)
  , disc_replication[,cols] %>% as.data.frame(stringsAsFactors=F)
  , delphi_reanalysis[,cols] %>% as.data.frame(stringsAsFactors=F)
  , disc_reanalysis[,cols] %>% as.data.frame(stringsAsFactors=F)
) %>% 
  group_by(task, trial, communication, analysis, dataset) %>%
  mutate(
    N = n()
  ) %>% 
  ungroup %>% 
  mutate(
    #  mu1 = mean(pre_influence, na.rm=T)
      alpha_back = (post_influence - soc_info)/(pre_influence-soc_info)
    , alpha = (pre_influence - post_influence)/(pre_influence-soc_info)
    
    #, alpha = ifelse(alpha<0, 0, alpha) 
    #, alpha = ifelse(alpha>1, 1, alpha)
  
    , stubborn_cent = 1-alpha
    , err = abs(pre_influence - truth)
    , err_norm = abs(err/truth)
    #, log_err_norm = log(pre_influence/truth)
  ) %>% ungroup() %>%
  mutate(
    valid = !is.na(pre_influence) & !is.na(post_influence)
  )



aggreg =  d %>%
  group_by(task, trial, communication, analysis, dataset) %>%
  mutate(
      mu1 = mean(pre_influence[valid])
    , toward_truth = ifelse((pre_influence < mu1 & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  ) %>%
  summarize(
    truth=unique(truth)
    , N_valid = length(pre_influence[valid])
    , N=unique(N)
    
    ## calc mean
    , mu1 = mean(pre_influence[valid])
    , mu2 = mean(post_influence[valid])
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth

    ## organizing stats
    , prop_toward = mean(toward_truth[valid]=="Toward")

    ### CHAT STATS
    
    , total_chat = sum(count_chat)
    , gini_talkativeness = Gini(count_chat[count_chat>0], na.rm=T)

    ### is the most talkative person toward truth?
    , central_twd_truth = ifelse(sum(!is.na(count_chat))==0, NA, toward_truth[!is.na(toward_truth)][which.max(count_chat[!is.na(toward_truth)])]=="Toward")

  ) %>%
  mutate(
      prop_toward_round=round(prop_toward,1)
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
    
  )



### DATA PRESENTATION TOOLS
nice_theme = function() {
  theme_test() +
    theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1.2)))
}

pct_labels = function(x) { paste0(x*100,"%")}
