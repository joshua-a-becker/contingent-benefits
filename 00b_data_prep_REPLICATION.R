### SET WORKING DIRECTORY TO
### THE DIRECTORY CONTAINING THIS SCRIPT

library(tidyverse, warn.conflicts = F, quietly = T)


delphi_replication = read.csv("Replication Data/delphi_data.csv", stringsAsFactors=F) %>%
  mutate(
      task=question
    , pre_influence=response_1
    , post_influence=response_5
    , communication="Delphi"
    , count_chat = NA
    , analysis="replication"
    , dataset="replication"
  ) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )




### get count chat for discussion data
chat_data_replication = read.csv("Replication Data/chat_data_anonymized.csv", stringsAsFactors=F)
Encoding(chat_data_replication$text)<-"bytes"
chat_data_replication = chat_data_replication %>%
  mutate(
     text = gsub("\x92", "x", text)   ### these all broke
    ,text = gsub("\x97", "x", text)  ### when i switched to 
    ,text = gsub("\xa33", "x", text) ### a macbook!
  ) %>%
  group_by(trial, playerId) %>%
  summarize(
      count_chat = sum(!is.na(text))
  )


disc_replication_nochat = read.csv("Replication Data/discussion_data.csv", stringsAsFactors=F) %>%
  mutate(
      task=question
    , pre_influence=initial
    , post_influence=final
    , communication="Discussion"
    , tr=substr(trial, 1, 15)
    , analysis="replication"
    , dataset="replication"
  )

disc_replication = disc_replication_nochat %>%
  merge(chat_data_replication, by=c("playerId","trial")) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )


data_loaded=14159