if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(lmerTest)

###########



repd = subset(aggreg, analysis=="replication")
reanal = subset(aggreg, analysis=="reanalysis")


mod1 = glm(improve ~ dataset + prop_toward, family="binomial", data=aggreg)

mod2 = glm(improve ~ dataset + var + prop_toward, family="binomial", data=aggreg)

mod3 = glm(improve ~ dataset + ind_err + prop_toward, family="binomial", data=aggreg)

mod4 = glm(improve ~ dataset + prop_toward*communication, family="binomial", data=aggreg)

mod5 = glm(improve ~ dataset + var + prop_toward*communication, family="binomial", data=aggreg) 

mod6 = glm(improve ~ dataset + ind_err + prop_toward*communication, family="binomial", data=aggreg) 


### CONTROLLING WITH INTERACTION 

## TABLE S1
htmlreg( 
  list(mod1, mod2, mod3, mod4, mod5, mod6)
, file="Figures/Control_vars.html"
#, custom.header=list("Reanalysis"=1:2,"Replication"=3:4)
#,custom.model.names=c("Numeric","Discussion", "Interaction")
, custom.coef.map=list(
  "(Intercept)"=NA
  , "var"="Variance"
  , "ind_err"="Individ. Err."
  , "prop_toward" = "&Phi;"
  , "communicationDiscussion" = "Mode (Discussion)"
  ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
)
, caption="Improvement as a function of &Phi;"
, caption.above=T
, stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)

