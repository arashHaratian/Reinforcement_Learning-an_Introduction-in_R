library(tidyverse)
library(reshape2)

# V <- vector("double", 101)
# V[101] <- 1
# prob <- 0.45


backup_action <- function(state, action){
  # state <- state+1
  if(state + action >= 102 | state - action <= 0)
  #   browser()
    return(0)
  val <- (prob * V[state + action]) + ((1 - prob) * V[state - action])
  if(is.na(val))
    browser()
  return(val)
}


vi <- function(){
  sweep_history <- list()
  count <- 1
  sweep_history[[count]] <- V
  
  while(TRUE){
    old_v <- V
    
    for(state in 2:100){
      vals <- map_dbl(2:min(state, 101-state), backup_action, state = state)
      V[state] <<- max(vals)
    }
    count <- count + 1
    sweep_history[[count]] <- V
    if(max(abs(old_v - V)) < 1e-8)
      break
  }
  return(sweep_history)
}

policy <- function(state){
  actions <- 2:min(state, 101-state)
  vals <- map_dbl(actions, backup_action, state = state)
  best_action <- actions[which.max(round(vals, 7))]
  return(best_action)
}



# plotting figure 4.3
plot_fig4.3.1 <- function() {
  V <<- vector("double", 101)
  V[101] <<- 1
  prob <<- 0.45
  
  vi_result <- vi()
  
  plot1 <- vi_result %>% 
    as.data.frame %>% 
    rename_with(.cols = everything(), .fn = ~paste0("sweep_", 1:16)) %>%
    mutate("capital" = 0:100) %>% 
    melt(measure.vars = paste0("sweep_", 1:16)) %>% 
    ggplot(aes(capital, value, group = variable, color = variable)) +
    geom_line() +
    labs(color = "", y = "value estimates")
  
  return(plot1)
}


plot_fig4.3.2 <- function() {
  plot2 <- map_dbl(1:100, policy) %>% 
    as.data.frame %>% 
    rename(stake := `.`) %>% 
    mutate("capital" = 0:99) %>% 
    ggplot(aes(capital, stake)) +
    geom_line() +
    labs(color = "", y = "final policy (stake)")
  
  return(plot2)
}


# plot_fig4.3.1()
# plot_fig4.3.2()
