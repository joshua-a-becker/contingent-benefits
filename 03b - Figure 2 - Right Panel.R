### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT

if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

library(lme4)
library(MASS)


### fit the model
disc_data = aggreg %>% 
  subset(!is.na(gini_talkativeness))

estimated_model_disc = disc_data %>% 
  glmer(improve ~ prop_toward*gini_talkativeness + (1|dataset), family="binomial", data=.)


mu_intercept_disc = sum(coef(estimated_model_disc)$dataset[,1]*(table(disc_data$dataset)/nrow(disc_data)))
mod_mu_disc = c(mu_intercept_disc, as.numeric(coef(estimated_model_disc)$dataset[1,2:4]))


### extract uncertainty around coefficients
mod_vcov_disc = vcov(estimated_model_disc)


### function for calculating sample error
### for specific values of the data
getSimDisc = function(prop_toward=0.5, gini_talkativeness=0.5, n=10000){
  
  ### draw sample of model coefficients based on uncertainty
  B_samp =  mvrnorm(n = n, mod_mu_disc, mod_vcov_disc)  
  
  samp_effect = B_samp[,"(Intercept)"] + B_samp[,"prop_toward"]*prop_toward +
    B_samp[,"gini_talkativeness"]*gini_talkativeness +
    B_samp[,"prop_toward:gini_talkativeness"]*prop_toward*gini_talkativeness
  
  
  samp_effect
}


## prep output dataframe
outDisc=data.frame(
    gini_talkativeness=numeric()
  , phi=numeric()
  , sd=numeric()
  , est=numeric()
  , fitest=numeric()
)


## calculate effect size (estimate) & sample error for each value of phi
## and each of the two conditions (commDisc)
for(gini_talkativeness in c(0.2, 0.5)){
  for(phi in seq(0,1,by=0.1)) {
    
    est_effect = mu_intercept_disc + 
      coef(estimated_model_disc)$dataset[1,"prop_toward"]*phi +
      coef(estimated_model_disc)$dataset[1,"gini_talkativeness"]*gini_talkativeness +
      coef(estimated_model_disc)$dataset[1,"prop_toward:gini_talkativeness"]*gini_talkativeness*phi
    
    
    samp_effect=getSimDisc(phi, gini_talkativeness)
    outDisc[nrow(outDisc)+1,]=c(gini_talkativeness
                          , phi
                          , sd(samp_effect)
                          , est_effect
                          , mean(samp_effect)
    )
  }
}



# outDisc %>%
#   mutate(
#     conf = as.numeric(sd)*1.96
#     , est=as.numeric(est)
#     , est_adj = exp(est)/(1+exp(est))
#     , upper = exp(est+conf)/(1+exp(est+conf))
#     , lower = exp(est-conf)/(1+exp(est-conf))
#     , phi=as.numeric(phi)
#   )  %>% View
library(scales)

my_colors = hue_pal()(3)[2:3]
outDisc %>%
  mutate(
      conf = as.numeric(sd)*1.96
    , est=as.numeric(est)
    , est_adj = exp(est)/(1+exp(est))
    , upper = exp(est+conf)/(1+exp(est+conf))
    , lower = exp(est-conf)/(1+exp(est-conf))
    , phi=as.numeric(phi)
  ) %>%
  mutate(
    ### this is just to make the figure the right size.....
    gini_labels = ifelse(gini_talkativeness<0.5, "Low", "Discussion")
  ) %>%
  ggplot(aes(x=phi, y=est_adj, color=factor(gini_labels)
             , group=factor(gini_labels)
             , fill=factor(gini_labels))) +
  geom_line() +
  geom_ribbon(aes(ymax=upper
                  , ymin=lower
                  , color=giniHighTrue
  ), alpha=0.1, color=NA) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  nice_theme() +
  labs(y="Prob(Improve)", x="\u03d5", fill="", color="") +
  # ylim(c(0.046,0.947))+
  theme(axis.text = element_text(size=7)) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors)

ggsave("Figures/Fig 2_right_centralization.png", width=2.8, height=1.5, dpi=500)



