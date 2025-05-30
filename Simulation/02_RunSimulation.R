rm(list=ls());gc()
library(igraph)
library(tidyverse)
library(parallel)
source("00_DeGroot Model.R")




if(!file.exists("myObj.Rds")) {
  source("01_MakeVector.R")
}

myObj=readRDS("myObj.Rds")

data=myObj["data"][[1]]
l=myObj["l"][[1]]
t=myObj["t"][[1]]
question <- data %>% pull(task) %>% unique()


  
runRound = function(i=1, list_of_alpha=1:4){
  N=20
  
  gen_alpha = function(N,class) {
    if(class==1) {
      x=runif(N, 0, 1) / 2
    }
    
    if(class==2) {
      x=runif(N, 0, 1)
    }
    
    if(class==3) {
      x=rbeta(N, 1, 3)
    }
    
    if(class==4) {
      x=rbeta(N, 3, 1)
    }
    
    if(class==5) {
      x=runif(N,0.5,1)
    }
    
    if(class==6) {
      x=runif(N, 0.45, 0.55)
    }
    
    
    x
  }
  
  
  
  alpha_class = c("Unif. (0, 0.5)", "Unif. (0, 1)", "Beta (1, 3)", "Beta (3, 1)","runif(N,0.5,1)", "Fixed 0.5")
  #print(i)
  mydf = results_template
  for(this_alpha_class in list_of_alpha) {
    q = sample(question, 1)
 
    tr=t[[q]]

    x = sample(l[[q]], N, replace=T)
    
    pre_mean = mean(x)
    err=abs(x-tr)
  
    alpha=gen_alpha(N, this_alpha_class)
    
    phi = ifelse(tr>mean(x),
      mean(x>mean(x)),
      mean(x<mean(x))
    )
    
    g = graph.full(n=N)#sample_pa(n=N, power=3, m=2, directed=F)
    V(g)$guess=x
    V(g)$alpha = alpha
    
    mydf[nrow(mydf)+1,]=simDegroot(g, 3, TRUTH=tr) %>%
      mutate(
         question = q
        ,  pre_mean = pre_mean
        , phi = phi
        , change = mean - pre_mean
        , net="full"
        , alpha=this_alpha_class
      ) %>% 
      tail(n=1)
     
    
    g = sample_pa(n=N, power=5, m=1, directed=F)
    V(g)$guess=x
    V(g)$alpha = alpha
    
    mydf[nrow(mydf)+1,]=simDegroot(g, 3, TRUTH=tr) %>%
      mutate(
        question = q
        ,  pre_mean = pre_mean
        , phi = phi
        , change = mean - pre_mean
        , net="centralized"
        , alpha=this_alpha_class
      ) %>% 
      tail(n=1)
    
  }
  #system(sprintf('echo "\n%s\n"', i))
  mydf
} 

results_template=setNames(data.frame(matrix(ncol = 15, nrow = 0)), c(
  'N','deg.cent','truth','mean','median','mean_ind_err','core','sd.pool','round','question'
  ,'pre_mean','phi','change','net','alpha'
))

my_alpha = c(2,4,6)

num_cores = 6 ### SET THIS VALUE
if(Sys.info()['sysname']=="Windows") {
  cl <- makeCluster(getOption("cl.cores", 14))
  clusterExport(cl,"runRound")
  clusterExport(cl,"data")
  clusterExport(cl,"l")
  clusterExport(cl,"t")
  clusterExport(cl,"question")
  clusterExport(cl, "results_template")
  clusterExport(cl,"simDegroot")
  clusterExport(cl, "my_alpha")
}

results_file = "results.csv"

write.csv(results_template, results_file, row.names=F)

for(i in 1:100000) {
  results = NA
  if(Sys.info()['sysname']=="Windows") {
    results = do.call(rbind, 
                            parLapply(cl, 1:12, function(j){
                              library(igraph)
                              library(tidyverse)
                              runRound(j,my_alpha)
                            })
              )
  } else {
    results = do.call(rbind, mclapply(1:12, function(j){runRound(j,my_alpha)}, mc.cores=mc.cores))
  }  
  write.table(results, results_file, append=T, col.names=F, sep=",", row.names=F)
  print(i)
}
