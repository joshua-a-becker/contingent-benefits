### fit the model
mod1_numeric = aggreg %>% 
  subset(dataset=="replication2" & communication=="Delphi") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod1_disc = aggreg %>% 
  subset(dataset=="replication2" & communication=="Discussion") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod2 = aggreg %>% 
  subset(dataset=="replication2") %>%
  glm(improve ~ prop_toward*communication, family="binomial", data=.)

summary(mod1_numeric)
estWithCi(mod1_numeric)

summary(mod1_disc)
estWithCi(mod1_disc)

summary(mod2)
estWithCi(mod2, param="prop_toward:communicationDiscussion")

# B_numeric = -0.6 [-4.0, 2.9]
# B_disc = 0.53 [-2.4, 3.4]
# B_interaction = 1.11 [-3.4, 5.6]



## TABLE S1
htmlreg(
  list(mod1_numeric, mod1_disc, mod2)
  , file="Figures/Table S1_controlled_phi.html"
  #, custom.header=list("Reanalysis"=1:2,"Replication"=3:4)
  ,custom.model.names=c("Numeric","Discussion", "Interaction")
  , custom.coef.map=list(
    "(Intercept)"=NA
    , "prop_toward" = "&Phi;"
    , "communicationDiscussion" = "Mode (Discussion)"
    ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
  )
  , caption="Improvement as a function of &Phi;"
  , caption.above=T
  , stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)
