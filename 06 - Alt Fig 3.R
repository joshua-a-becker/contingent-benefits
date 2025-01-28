library(lme4)

estimated_model = aggreg %>%
  subset(communication=="Discussion") %>%
  glm(improve ~ central_twd_truth + dataset, family="binomial", data=.)

summary(estimated_model)

myd = aggreg %>%
  subset(communication=="Discussion")




intercept = coef(estimated_model)["(Intercept)"]
datasetmu = coef(estimated_model)["datasetreplication"]*mean(myd$dataset=="replication")

mod_mu = c(coef(estimated_model)[1], coef(estimated_model)[2], datasetmu)

mod_vcov = vcov(estimated_model)

getSim = function(central_twd_truthTRUE=1, n=1000){
  
  ### draw sample of model coefficients based on uncertainty
  B_samp =  mvrnorm(n = n, mod_mu, mod_vcov)  
  
  samp_effect = B_samp[,"(Intercept)"] + 
    B_samp[,"central_twd_truthTRUE"]*central_twd_truthTRUE +
    B_samp[,"datasetreplication"]
  
  
  samp_effect
}

out=data.frame(
    central_twd_truthTRUE=numeric()
  , sd=numeric()
  , est=numeric()
  , fitest=numeric()
)


for(central_twd_truthTRUE in c(0,1)) {
  
  est_effect = intercept + 
    datasetmu +
    coef(estimated_model)["central_twd_truthTRUE"]*central_twd_truthTRUE
  
  samp_effect=getSim(central_twd_truthTRUE = central_twd_truthTRUE)
  out[nrow(out)+1,]=c(  central_twd_truthTRUE
                        , sd(samp_effect)
                        , est_effect
                        , mean(samp_effect)
  )
}



out %>%
  mutate(
      conf = as.numeric(sd)*1.96
    , est=as.numeric(est)
    , est_adj = exp(est)/(1+exp(est))
    , upper = exp(est+conf)/(1+exp(est+conf))
    , lower = exp(est-conf)/(1+exp(est-conf))
  ) %>%
  ggplot(aes(x=central_twd_truthTRUE==1, y=est_adj)) +
  geom_point() +
  geom_errorbar(aes(ymax=upper, ymin=lower), width=0) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  ylim(c(0,1))

#ggsave("Figures/Alt Fig 3.png", width=2.8, height=1.5)


