if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

library(texreg)
library(lme4)

estWithCi = function(mod, param="prop_toward", mult=1.96) {
  c(coef(summary(mod))[param,"Estimate"]
  ,coef(summary(mod))[param,"Estimate"] - coef(summary(mod))[param,"Std. Error"]*mult
  ,coef(summary(mod))[param,"Estimate"] + coef(summary(mod))[param,"Std. Error"]*mult
  ,coef(summary(mod))[param,"z value"]
  )
}

### fit the model
mod1_numeric = aggreg %>% 
  subset(communication=="Delphi" & dataset=="lorenz2011") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod1_disc = aggreg %>% 
  subset(communication=="Discussion" & dataset=="gurcay2015") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod2 = aggreg %>% 
  subset(dataset %in% c("gurcay2015","lorenz2011")) %>%
  glm(improve ~ prop_toward*communication, family="binomial", data=.)




## TABLE S1
htmlreg( 
  list(mod1_numeric, mod1_disc, mod2)
, file="Figures/Table Regression_small_groups.html"
#, custom.header=list("Reanalysis"=1:2,"Replication"=3:4)
,custom.model.names=c("Numeric","Discussion","Interaction")
, custom.coef.map=list(
  "(Intercept)"=NA
  , "prop_toward" = "&Phi;"
  , "communicationDiscussion" = "Mode (Discussion)"
  ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
)
, caption="Improvement as a function of &Phi;<br>Small Group Data ONLY"
, caption.above=T
, stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)
