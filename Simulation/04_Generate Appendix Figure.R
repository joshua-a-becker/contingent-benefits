rm(list=ls()); gc()

library(tidyverse)
results=read.csv("resultsx.csv")

alpha_class = c("Unif. (0, 0.5)", "Unif. (0, 1)", "Beta (1, 3)", "Beta (3, 1)","runif(N,0.5,1)", "Unif (0.45, 0.55)")

dsum = results %>%
  # mutate(
  #   phi = round(phi*5)/5
  # ) %>%
  group_by(phi, net, truth, alpha) %>%
  mutate(
    err1 = abs(pre_mean-truth)
    , err2= abs(mean-truth)
    , improve = err2<err1
  ) %>%
  group_by(phi, net, alpha) %>%
  summarize(
    improve = mean(improve)
    , n=n()
  ) %>%
  mutate(
    ci = sqrt(improve*(1-improve)/n)*1.96
    , upper = improve+ci
    , lower = improve-ci
    , alpha_class = alpha_class[alpha]
  )


mytheme = theme(panel.background=element_rect(fill="white", color="black", size=1.1), 
                axis.text=element_text(size=rel(1), color="black"), 
                strip.text=element_text(size=rel(1.1)), 
                legend.text=element_text(size=rel(1.1)), strip.background=element_blank(),
                title=element_text(size=rel(1.1)),
                panel.grid=element_blank(),
                plot.title=element_text(hjust=0.5))



dsum %>%
  mutate(
    communication=factor(net, levels=c("full", "centralized"))
    , communication = fct_recode(communication, Numeric="full", Discussion = "centralized")
  ) %>%
  # subset(phi>=0.15 & phi<=0.85) %>%
  ggplot(aes(x=phi, y=improve, color=communication)) +
  #geom_point() + 
  geom_line() +
  #geom_errorbar(aes(ymin = lower, ymax=upper), width=0) +
  geom_hline(yintercept=0.5) + geom_vline(xintercept=0.5) +
  facet_wrap(.~alpha_class, scales="free") +
  #lemon::coord_capped_cart(bottom='both') +
  scale_x_continuous(breaks=seq(0.1,0.9,by=0.2), limits=c(0.1, 0.9), labels=scales::percent)+
  scale_y_continuous(breaks=seq(0.1,0.9,by=0.2), limits=c(0.1, 0.9), labels=scales::percent)+
  mytheme +
  xlab("\u03d5")+
  ylab("Prob(Improve)")

ggsave("Simulated_Appendix.png", width=7.5, height=2.3)


