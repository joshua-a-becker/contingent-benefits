library(scales)
if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}


aggreg_named = aggreg %>%
  mutate(dataset=factor(dataset))

levels(aggreg_named$dataset) = c("Lorenz, 2011", "Becker, 2017", "Becker, 2019", "Gurcay, 2015", "Replication")

aggreg_named %>%
  mutate(phi=round(prop_toward,1))%>%
  group_by(dataset) %>%
  mutate(
    total = n()
  ) %>%
  ungroup() %>%
  group_by(dataset, phi) %>%
  summarize(
    tot_phi = n()
    ,pct = n()/unique(total)
  ) %>%
  select(dataset, phi, pct, tot_phi) %>% 
  ggplot(aes(x=phi, y=pct)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = percent_format(), lim=c(0,0.49))+
  scale_x_continuous(labels = percent_format(), lim=c(-0.01, 1.05)) +
  facet_wrap(.~dataset, scales="free") +
  nice_theme() +
  ylab("") + xlab("\nDistribution of \u03d5 by Dataset")+ 
  theme(plot.margin = margin(1, 10, 1, 1))

ggsave("figures/Phi_Histogram.png", width=5.3, height=3.8)
