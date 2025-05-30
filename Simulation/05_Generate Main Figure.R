rm(list=ls());gc()
library(tidyverse)
results = read.csv("results.csv")

nice_theme = function() {
  theme_test() +
    theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1.2)))
}

pct_labels = function(x) { paste0(x*100,"%")}


### fit the model
logistic_results = results %>%
  subset(alpha==2) %>%
  mutate(
      err1 = abs(pre_mean-truth)
    , err2= abs(mean-truth)
    , improve = err2<err1
  )

## Beta (3, 1)

estimated_model = logistic_results %>% 
  glm(improve ~ phi*net, family="binomial", data=.)

disc_fit = coef(estimated_model)["(Intercept)"] +
  coef(estimated_model)["phi"]*seq(0, 1, by=0.1)

numeric_fit = coef(estimated_model)["(Intercept)"] +
  coef(estimated_model)["phi"]*seq(0, 1, by=0.1)+
  coef(estimated_model)["netfull"] + 
  coef(estimated_model)["phi:netfull"]*seq(0, 1, by=0.1)




rbind(
    data.frame(communication="Numeric", est = numeric_fit, phi=seq(0, 1, by=0.1))
  , data.frame(communication="Discussion", est = disc_fit, phi=seq(0, 1, by=0.1))
) %>%
  mutate(
      communication=factor(communication, levels=c("Numeric","Discussion"))
    ,  est=as.numeric(est)
    , est_adj = exp(est)/(1+exp(est))
    , phi=as.numeric(phi)
  )  %>%
  ggplot(aes(x=phi, y=est_adj, color=communication
             , group=communication
             , fill=communication)) +
  geom_line() +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  ylim(c(0,1))+
  xlim(c(0,1))+
  nice_theme() +
  labs(y="Prob(Improve)", x="\u03d5", fill="", color="") +
  theme(axis.text = element_text(size=7))

ggsave("../Figures/Simulated_Logistic.png", width=2.8, height=1.5, dpi=500)
