rm(list=ls())

library(tidyverse)

setwd("../")
source("00c_data_prep_MERGE.R")
setwd("Simulation")

data <- d %>%
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  ungroup() %>% mutate(task=paste0(task,truth))%>%select(task,pre_influence, post_influence,truth)

# We need a vector for each question/task that contains all the first estimates (i.e. pre_influence)

# First, let's extract the unique questions
question <- data %>% pull(task) %>% unique()
question
# Then, let's create a list where we'll add the vectors
l <- list()
t <- list()
for(i in question){
  l[[i]] <-  data %>% 
    filter(task == i) %>% 
    pull(pre_influence) # pull takes the selected column and turns it into a vector
  
  t[[i]] <-  data %>% 
    filter(task == i) %>% 
    pull(truth) %>% unique # pull takes the selected column and turns it into a vector
  
  
} 
l # it creates a vector of vectors so to speak that will also retain the name of the question/task

# example showing you can loop through the vectors (it just requires a bit of a workaround as lists have a specific organisation and calling method)
for(q in question){
  x=l[[q]]
  tr=t[[q]]
  print(min(x))
  print(c("t=",tr))
}


saveRDS(list(l=l, data=data, t=t), "myObj.Rds")
