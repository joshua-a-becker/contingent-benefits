### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT


if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(tidyverse)
#### combined 

aggreg %>%
  subset(communication=="Discussion") %>%
  ### remove those 5 trials with no chat data
  subset(!is.na(central_twd_truth)) %>%
  mutate(
    #improve = ifelse(improve, "Improve","Worse")
    central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
    #, analysis= ifelse(analysis=="replication","Replication","Reanalysis")
  ) %>% 
  group_by(central_twd_truth) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(x=central_twd_truth, y=improve#, color=analysis
  )) +
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(0.5)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_y_continuous(labels=pct_labels, lim=c(0.11,0.89), breaks=c(0.2,0.4,0.6,0.8))+
  labs(x="Central Node Toward Truth", y="% Improve") +
  theme_test() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(hjust = 0),
        strip.text = element_text(face = "bold", color = "black", size = 12)) + 
  theme(plot.margin = unit(c(5.5, 30, 5.5, 5.5), "pt") )

ggsave("Figures/Central Node Predicts.png", width=2, height=2)


#### separate datasets

aggreg %>%
  subset(communication=="Discussion") %>%
  ### remove those 5 trials with no chat data
  subset(!is.na(central_twd_truth)) %>%
  mutate(
    #improve = ifelse(improve, "Improve","Worse")
    central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
    #, analysis= ifelse(analysis=="replication","Replication","Reanalysis")
  ) %>% 
  group_by(central_twd_truth, analysis) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(x=central_twd_truth, y=improve#, color=analysis
  )) +
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(0.5)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_y_continuous(labels=pct_labels, lim=c(0.11,0.89), breaks=c(0.2,0.4,0.6,0.8))+
  facet_grid(.~analysis)+
  labs(x="Central Node Toward Truth", y="% Improve") +
  theme_test() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(hjust = 0),
        strip.text = element_text(face = "bold", color = "black", size = 12))

ggsave("Figures/Central Node Predicts_separate.png", width=3, height=2)
