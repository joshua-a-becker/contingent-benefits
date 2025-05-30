### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT

if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}



### THEN:  DOES CONVERSATIONAL CLUSTERING REDUCE EFFECT OF PHI?
navajas_data = read.csv("Reanalysis Data/navajas_et_al.csv", stringsAsFactors=F) %>%
  mutate(
    communication="small_group"
    , analysis="reanalysis"
    , dataset="navajas"
    , soc_info=NA
    , count_chat=NA
    , count_words=NA
  )


navajas_aggreg_original = navajas_data %>%
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  group_by(task, trial) %>%
  mutate(
    mu1 = mean(pre_influence)
    # , err1 = abs(pre_influence - truth)
    # , err2 = abs(post_influence - truth)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  ) %>%
  group_by(task, trial) %>%
  summarize(
    truth=unique(truth)
    , n = n()
    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    
    ## error of mean -- did it improve?
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , improve=(change_err_mu<0)*1
    
    ## organizing stats
    # , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
  ) %>%
  mutate( 
    dataset="navajas"
    , task=as.character(task)
    , trial=as.character(trial)
  )



## set up DF for bootstrap results
boot_out = data.frame(intercept=numeric(), prop_toward=numeric(), netsparse=numeric(), interaction=numeric())

### run many bootstrap iterations
options(dplyr.summarise.inform = FALSE)
for(boot in 1:10000) {
  print(boot)
  
  ### first, randomly cluster groups
  ### and calc group outcomes as per randomly assigned nomimal group ID
  
  ### for each bootstrap run
  ### we randomly shuffle trial labelling
  ### to create synthetic large groups out of small groups
  shuffle_clusters = function(cluster_labels) {
    original_labels <- cluster_labels
    new_labels <- sample(unique(original_labels))
    relabeled <- match(original_labels, unique(original_labels))
    relabeled <- new_labels[relabeled]
    relabeled
  }
  
  navajas_aggreg_synthetic = navajas_data %>%
    mutate(
      trial=floor((shuffle_clusters(trial)-1)/4)
    ) %>% 
    subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
    group_by(task, trial) %>%
    mutate(
      mu1 = mean(pre_influence)
      , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
    ) %>%
    group_by(task, trial) %>%
    summarize(
      truth=unique(truth)
      , n = n()
      ## calc mean
      , mu1 = mean(pre_influence)
      , mu2 = mean(post_influence)
      
      ## error of mean -- did it improve?
      , err_mu1 = abs(mu1 - truth)
      , err_mu2 = abs(mu2 - truth)
      , change_err_mu = mean(err_mu2 - err_mu1)/truth
      , improve=(change_err_mu<0)*1
      
      ## organizing stats
      # , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
      , prop_toward = mean(toward_truth=="Toward")
    ) %>%
    mutate( 
      dataset="navajas"
      , task=as.character(task)
      , trial=as.character(trial)
    )
  
  
  
  ##### extract relevant columns, label networks, and RESAMPLE for bootstrap
  
  core_cols=c("dataset","trial","improve","prop_toward")
  
  ## the original data, formatted conveniently
  fc = subset(aggreg, communication=="Discussion" & dataset=="replication")[,core_cols] %>% mutate(net="full")
  
  ## synthetic groups, formatted conveniently
  sparse = navajas_aggreg_synthetic[,core_cols] %>% mutate(net="sparse")
  
  ## for proper bootstrap variance, we now sample the datasets
  sparse_boot = sparse[sample(1:nrow(sparse), replace=T),]
  fc_boot = fc[sample(1:nrow(fc), replace=T),]
  
  ## compare large group vs synthetic sparse groups
  disc_comp = rbind(fc_boot, sparse_boot) 
  mod_comb = glm(improve~prop_toward*net, data=disc_comp, family="binomial")
  boot_out[nrow(boot_out)+1,] = coef(mod_comb)
}

results = boot_out %>%
  summarize(
    intercept_est = mean(intercept)
    , intercept_se = sd(intercept)
    , prop_toward_est = mean(prop_toward)
    , prop_toward_se = sd(prop_toward)
    , prop_toward_lower = prop_toward_est - prop_toward_se*1.96
    , prop_toward_upper = prop_toward_est + prop_toward_se*1.96
    , netsparse_est = mean(netsparse)
    , netsparse_se = sd(netsparse)
    , netsparse_upper = netsparse_est+1.96*netsparse_se
    , netsparse_lower = netsparse_est-1.96*netsparse_se
    , interaction_est = mean(interaction)
    , interaction_se = sd(interaction)
    , interaction_upper_95 = interaction_est + 1.96*interaction_se
    , interaction_lower_95 = interaction_est - 1.96*interaction_se
    , interaction_upper_99 = interaction_est + 2.58*interaction_se
    , interaction_lower_99 = interaction_est - 2.58*interaction_se
    
  ) %>% t

#### B_full
results["prop_toward_est",]

#### B_breakout
results["prop_toward_est",] + results["interaction_est",]

#### is interaction term significantly differnt from zero
#### for 99% confidence interval?  (P<0.01)
results["interaction_upper_99",]
