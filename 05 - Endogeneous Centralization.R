disc = aggreg %>%
  subset(!is.nan(gini_talkativeness))

disc %>% 
  glmer(improve ~ prop_toward*gini_words + N + (1|dataset), family="binomial", data=.) %>%
  summary

disc %>% 
  glmer(improve ~ prop_toward*gini_talkativeness_present_only + (1|dataset), family="binomial", data=.) %>%
  summary


disc %>% 
  glmer(improve ~ prop_toward*freeman + (1|dataset), family="binomial", data=.) %>%
  summary

disc %>%
  ggplot(aes(x=freeman, y=gini_talkativeness)) + 
  geom_point()

disc %>%
  ggplot(aes(x=freeman, y=gini_talkativeness_present_only)) + 
  geom_point()

disc %>%
  ggplot(aes(x=gini_talkativeness, y=gini_talkativeness_present_only)) + 
  geom_point()


hist(disc$gini_talkativeness)
hist(disc$gini_talkativeness_present_only)

median(disc$gini_talkativeness)
median(disc$gini_talkativeness_present_only)

disc_rean %>% 
  mutate(
    gtsq = (gini_talkativeness^2)
  ) %>%
  glm(improve ~ prop_toward*gini_talkativeness, family="binomial", data=.) %>%
  summary

table(disc_rep)


cor(disc$gini_talkativeness_present_only, disc$N)


table(aggreg$communication)

odd=aggreg[which(is.nan(aggreg$gini_talkativeness) & aggreg$communication=="Discussion"),]


odd_chat = subset(chat_stats, dataset=="gurcay2015" & is.na(gini_talkativeness))

odd_trials = unique(odd_chat$trial)


odd_s = gurc_d %>%subset(trial %in% odd_trials) %>% pull(subject.no) %>% unique

chats %>% subset(subject.no %in% odd_s)








aggreg =  d %>%
  ungroup() %>%
  mutate(
    valid = !is.na(pre_influence) & !is.na(post_influence)
  ) %>% 
  group_by(task, trial, communication, analysis, dataset) %>%
  summarize(
    truth=unique(truth)
    , N_valid = length(pre_influence[valid])
    , N=unique(N)
    
    ## calc mean
    , mu1 = mean(pre_influence[valid])
    , mu2 = mean(post_influence[valid])
    
    ## cal median
    , med1 = median(pre_influence[valid])
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
    , var = var(pre_influence[valid])
    # , ind_err = mean((pre_influence-truth)^2)
    # , omega_hat_03 = calcOmegaHat(pre_influence, N, w=0.3, truth)
    
    , total_chat = sum(count_chat)
    
    ## centralization
    # , gini_alpha = Gini(stubborn_cent)
    # , gini_alpha = ifelse(is.na(gini_alpha), 0, gini_alpha)
    
    ## other alpha
    # , alpha_cor = cor.test(stubborn_cent[is.finite(stubborn_cent)], err[is.finite(stubborn_cent)], na.rm=T)$estimate
    # , move_cor = cor.test(err, (pre_influence==post_influence)*1)$estimate
    
    # , talk_cor = tryCatch(cor.test(err, count_chat)$estimate,error=function(e){NA})
    
    , gini_talkativeness = Gini(count_chat, na.rm=T)
    , gini_talkativeness_present_only = Gini(count_chat[count_chat>0], na.rm=T)
    , gini_words = Gini(count_words, na.rm=T)
    
    , cv_talkativeness = 100*sd(count_chat)/mean(count_chat, na.rm=T)
    , cv_words =  100*sd(count_words)/mean(count_words, na.rm=T)
    
    , pct_silent = mean(count_words==0, na.rm=T)
    , count_silent = sum(count_words==0, na.rm=T)
    
    , hhi_talkativeness = (sum((count_chat / sum(count_chat))^2)-1/N)/(1-1/N) 
    , entropy_talkativeness = -sum((p <- count_chat / sum(count_chat))[p > 0] * log(p[p > 0])) / log(length(count_chat))
    
    , mean_talkativeness = mean(count_chat)
    , mean_talkativeness_present_only = mean(count_chat[count_chat>0])
    , mean_words = mean(count_words)
    , total_talkativeness=sum(count_chat)
    , total_words = sum(count_words)
    , count_in_convo = sum(count_words!=0, na.rm=T)
    
    ### extra stuff
    , sd = sd(pre_influence)
    , sd_pool = unique(sd_pool)
    , change_mu_norm = (mu2-mu1)/sd
    
    ### is the most talkative person toward truth?
    #, central_twd_truth = ifelse(sum(!is.na(count_chat))==0, NA, toward_truth[which.max(count_chat)]=="Toward")
    , central_twd_truth = ifelse(sum(!is.na(count_chat))==0, NA, toward_truth[!is.na(toward_truth)][which.max(count_chat[!is.na(toward_truth)])]=="Toward")
    , central_twd_truth_words = ifelse(sum(!is.na(count_words))==0, NA, toward_truth[!is.na(toward_truth)][which.max(count_words[!is.na(toward_truth)])]=="Toward")
    , central_pre_influence = ifelse(sum(!is.na(count_chat))==0, NA, pre_influence[which.max(count_chat)])
    , central_diff_from_mu =  (central_pre_influence-mu1)/sd
  ) %>%
  mutate(
    prop_toward_round=round(prop_toward,1)
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
    
  )
