library(tidyverse)
library(reshape2)
library(patchwork)
source("./Chapter10/tiles3.R")    # or copy the source code


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


num_of_servers <- 10
priorities <- 1:4
rewards <- c(1, 2, 4, 8)


init_weights <- function(size_val = 2048){
  w <<- vector("double", size_val)
}


get_tiles <- function(num_free_servers,
                      priority,
                      action,
                      IHT,
                      assign_hash_function,
                      num_of_tilings = 8){
  
  server_scale <- num_of_tilings / num_of_servers
  priority_scale <- num_of_tilings / (length(priorities) - 1)
  
  num_free_servers <- server_scale * num_free_servers
  priority <- priority_scale * priority
  
  active_tiles <- tiles(IHT,
                        assign_hash_function,
                        num_of_tilings,
                        c(num_free_servers, priority),
                        action)
  return(active_tiles)
}

Q <- function(num_free_servers, priority, action, ...){
  active_tiles <- get_tiles(num_free_servers, priority, action, ...)
  return(sum(w[active_tiles]))
}


state_value <- function(num_free_servers, priority, IHT, assign_hash_function){
  action_values <- map_dbl(1:2, ~Q(num_free_servers, priority, .x, IHT, assign_hash_function))
  if(num_free_servers == 0)
    return(action_values[[1]]) # index one is for rejecting
  
  return(max(action_values))
}


policy <- function(num_free_servers, priority, IHT, assign_hash_function){
  action_values <- map_dbl(1:2, ~Q(num_free_servers, priority, .x, IHT, assign_hash_function))
  return(which.max(action_values))
}


episode_differential_semi_gradient_sarsa <- function(IHT,
                                                     assign_hash_function,
                                                     iterations = 2e+06,
                                                     alpha = 0.01,
                                                     beta = 0.01,
                                                     epsilon = 0.1,
                                                     free_probability = 0.06,
                                                     num_of_tilings = 8){
  
  average_reward <- 0
  
  # Initialization
  current_free_servers <- num_of_servers
  priority <- resample(priorities, 1)
  
  # choose an action
  if(current_free_servers == 0){
    action <- 1     # reject   
  } else if(runif(1) < epsilon){
    action <- resample(1:2, 1)
  } else{
    values <- map_dbl(1:2, ~Q(current_free_servers, priority,
                              .x,
                              IHT = IHT,
                              assign_hash_function = assign_hash_function))
    
    action <- resample(which(values == max(values)), 1)
  }
  
  
  # page 251
  for(i in seq_len(iterations)){
    
    # taking action
    if(current_free_servers > 0 & action == 2){
      # (s prime)
      new_free_servers <- current_free_servers - 1
    } else{
      new_free_servers <- current_free_servers
    }
    
    reward <- 0
    
    if(action == 2)
      reward <- rewards[[priority]]
    
    busy_servers <- num_of_servers - new_free_servers
    new_free_servers <- new_free_servers + rbinom(1, busy_servers, free_probability)
    
    new_priority <- resample(priorities, 1)
    
    
    # choose a new action (A prime)
    if(new_free_servers == 0){
      new_action <- 1     # reject   
    } else if(runif(1) < epsilon){
      new_action <- resample(1:2, 1)
    } else{
      values <- map_dbl(1:2, ~Q(new_free_servers, new_priority,
                                .x,
                                IHT = IHT,
                                assign_hash_function = assign_hash_function))
      
      new_action <- resample(which(values == max(values)), 1)
    }
    
    # learning
    
    
    current_values <- # Q
      Q(current_free_servers, priority,
                        action,
                        IHT = IHT,
                        assign_hash_function = assign_hash_function)
    
    
    new_values <- #Q prime
      Q(new_free_servers, new_priority,
                 new_action,
                 IHT = IHT,
                 assign_hash_function = assign_hash_function)
    
    delta <- reward - average_reward + new_values - current_values
    average_reward <- average_reward + (beta * delta)
    
    delta <- delta * (alpha / num_of_tilings)
    active_tiles <- get_tiles(current_free_servers, priority,
                     action,
                     IHT = IHT,
                     assign_hash_function = assign_hash_function)
    
    w[active_tiles] <<- w[active_tiles] + delta
    
    
    current_free_servers <- new_free_servers
    priority <- new_priority
    action <- new_action
  }
}






# plotting the figure

plot_fig10.5 <- function(iterations = 2e+06){
  
  # initialization
  init_weights()
  
  IHT <- initialize_IHT(2048)
  hash_table <- IHT[[1]]
  assign_hash_table <- IHT[[2]]
  
  # run the algorithm
  episode_differential_semi_gradient_sarsa(hash_table, assign_hash_table, iterations)

  
  # first plot
  best_actions <- cross2(seq_len(num_of_servers), priorities) %>%
    transpose() %>%
    pmap_dbl(~policy(.x, .y, IHT = hash_table, assign_hash_function = assign_hash_table))
  plot1 <- expand_grid(priorities, "num_of_servers" = seq_len(num_of_servers)) %>% 
    mutate(action = factor(best_actions, labels = c("reject", "accept")), 
           num_of_servers = as.factor(num_of_servers),
           priorities = as.factor(priorities)) %>% 
    ggplot(aes(x = num_of_servers, y = reorder(priorities, desc(priorities)) )) +
    geom_tile(aes(fill = action)) +
    labs(title = "policy",
         x = "number of free servers",
         y = "Priority")
    
       
    
  # second plot
  values <- list()
  for(priority in priorities){
    index <- as.character(2 ^ (priority - 1))
    values[[index]] <- map_dbl(0:10,
                               ~ state_value(.x,
                                             priority,
                                             IHT = hash_table,
                                             assign_hash_function = assign_hash_table))
  }
  
  lines <- imap(values, ~geom_line(aes(y = .x, group = .y, color = .y)))
  
  plot2 <- data.frame("servers" = 0:10) %>% 
    ggplot(aes(x = as.factor(servers))) +
    lines +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "value function",
         x = "number of free servers",
         y = "differential value of best action",
         color = "priority")
  
  return(wrap_plots(plot1, plot2))
}



plot_fig10.5()
