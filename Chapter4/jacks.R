library(tidyverse)
library(reshape2)


V <- policy <- matrix(0, nrow = 21, ncol = 21)



load_P_and_R <- function(lambda_requests, lambda_dropoffs){
  requests <- 0
  R <- vector("double", length = 26)
  P <- matrix(0, nrow = 26, ncol = 21)
  while(TRUE){
    
    request_prob <- dpois(requests, lambda_requests)
    R <- R + map_dbl(.x = 0:25, ~(request_prob * min(.x, requests) * 10))
    
    dropoffs <- 0
    while(TRUE){
      drop_prob <- dpois(dropoffs, lambda_dropoffs)

      for(n in 0:25){
        satisfied_requests <- min(requests, n)
        new_n <- max(0, min(20, n + dropoffs - satisfied_requests))
        P[n + 1, new_n + 1] <- P[n + 1, new_n + 1] + (request_prob * drop_prob)
      }

      if(drop_prob < 1e-6)
        break
      dropoffs <- dropoffs + 1
    }
    
    if(request_prob < 1e-6)
      break
    requests <- requests + 1
  }
  
  return(list("P" = P, "R" = R))
}





loc1 <- load_P_and_R(lambda_requests = 3, lambda_dropoffs = 3)
P1 <- loc1$P
R1 <- loc1$R

loc2 <- load_P_and_R(lambda_requests = 4, lambda_dropoffs = 2)
P2 <- loc2$P
R2 <- loc2$R



backup_action <- function(n1, n2, action, discount = 0.9) {
  
  action <- min(action, n1) %>% 
    max(-n2) %>% 
    min(5) %>% 
    max(-5)
  
  morning_n1 <- n1 - action
  if(morning_n1 <= 0)
    return(0)
  # morning_n1 <- 1

  morning_n2 <- n2 + action 
  if(morning_n2 <= 0)
    return(0)
    # morning_n2 <- 1

  vals1 <- vector("double", 21)
  vals2 <- vector("double", 21)
  
  for(new_n1 in 1:21){ 
    for(new_n2 in 1:21){
      r <- R1[morning_n1] + R2[morning_n2] + discount * V[new_n1, new_n2]
      if(length(r)!= 1)
        browser()
      vals2[new_n2] <- P1[morning_n1, new_n1] * P2[morning_n2, new_n2] * r
    }
    vals1[new_n1] <- sum(vals2)
  }
  
  
  return(sum(vals1) + (abs(action) * -2))
}


policy_eval <- function(theta = 1e-7) {
  while (TRUE) {
    
    old_v <- V
    
    for(n1 in 1:21){
      for(n2 in 1:21){
        action <- policy[n1, n2]
        V[n1, n2] <<- backup_action(n1, n2, action)
      }
    }
    if(theta > max(abs(old_v - V)))
      break
  }
  invisible(V)
}



policy_function <- function(n1, n2) {
  actions <- max(-5, (-n2)):min(5, n1)
  action_vals <- map_dbl(actions, backup_action, n1 = n1, n2 = n2)
  return(actions[which.max(action_vals)])
}


greedify <- function() {
  
  is_policy_improved <- FALSE
  for (n1 in 1:21) {
    for(n2 in 1:21){
      b <- policy[n1, n2]
      policy[n1, n2] <<- policy_function(n1, n2)
      
      if(b != policy[n1, n2])
        is_policy_improved <- TRUE
    }
    # print(new_policy)
  }
  return(is_policy_improved)
}

policy_iteration <- function(){
  count <- 1
  policies <- list()
  while(TRUE){
    policies[[as.character(count)]] <- policy
    policy_eval()
    if(!greedify())
      break
    count <- count + 1
  }
  return(policies)
}



# plotting figure 4.2

plot_fig4.2 <- function(){
  result <- policy_iteration()
  
  
  plots <- map(.x = result,
               ~ggplot(melt(.x), aes(Var2, Var1)) +
                 geom_tile(aes(fill = value)) +
                 labs(x = "#cars at second location",y = "#cars at first location"))
  
  
  plots[["V"]] <- ggplot(melt(V), aes(Var2, Var1)) +
    geom_tile(aes(fill = value)) +
    labs(x = "#cars at second location",y = "#cars at first location")
  
  patchwork::wrap_plots(plots)
}

plot_fig4.2()
