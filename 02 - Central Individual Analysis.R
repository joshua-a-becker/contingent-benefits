### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT

if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}


# 2x2 proportion test
# probability of improvement as a fn of whether most talkative individual 
# is in correct/incorrect direction
# FOR DISCUSSION ONLY

comb_test = aggreg %>%
  subset(communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()


rean_test = aggreg %>%
  subset(analysis=="reanalysis" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()



rean_test = aggreg %>%
  subset(analysis=="reanalysis" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()


rep_test = aggreg %>%
  subset(analysis=="replication" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()


### print results

# the test
rean_test

#the estimate
rean_test$estimate[1]-rean_test$estimate[2]

# the test
rep_test
# the estimate
rep_test$estimate[1]-rep_test$estimate[2]



# the test
comb_test
# the estimate
comb_test$estimate[1]-comb_test$estimate[2]



################################################
## is talkativeness correlated with accuracy? ##
################################################


### prep data
myd = 
  d %>% subset(valid) %>%
  subset(is.finite(count_chat)
         & communication=="Discussion") %>%
  group_by(task) %>%
  mutate(
    err_quant = cut(err,
                    breaks=quantile(err, probs=seq(0,1,by=0.25)), include.lowest=T) %>% as.numeric
  ) %>%
  group_by(analysis) %>%
  mutate(
    talk_quant = cut(count_chat,
                     breaks=quantile(count_chat, probs=seq(0,1,by=0.25)), include.lowest=T) %>% as.numeric
  )

## run test for reanalysis
myd %>%
  subset(analysis=="reanalysis") %>%
  with(
    cor.test(talk_quant, err_quant)
  )

## run test for replication
myd %>%
  subset(analysis=="replication") %>%
  with(
    cor.test(talk_quant, err_quant)
  )

