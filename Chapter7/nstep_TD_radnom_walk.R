library(tidyverse)
library(reshape2)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


init_state_values <- function(terminal_state = 21){
  V <<- rep(0, terminal_state)
}


episode <- function(terminal_state = 21, n, alpha = 0.1, discount = 1) {
  
  current_state <- terminal_state %/% 2 + 1   # middle state is the first state
  
  states <- c(current_state)
  rewards <- c(0)
  
  time <- 0
  episode_length <- Inf
  
  while(TRUE){
  
    time <- time + 1
    
    if(time < episode_length){

      action <- resample(c(-1,1), 1) # next action on random
      
      current_state <- current_state + action
      
      reward <- 0
      if(current_state == 1)
        reward <- -1        # reward for ending on the left
      if(current_state == terminal_state)
        reward <- 1         # reward for ending on the right
      
      
      # storing new state and new reward
      states <- append(states, current_state)
      rewards <- append(rewards, reward)
      
      
      if(current_state == terminal_state | current_state == 1)
        episode_length <- time
      
    }
    
    # n-step TD, page 144
    tau <- time - n
    
    if(tau >= 0){
      range <- (tau + 1):(min(tau + n, episode_length) + 1) 
      G <- 0   # returns
      for(i in range){
        G <- G + (discount ^ (i - tau - 1)) * rewards[i]
      }
      
      if(tau + n <= episode_length){
        G <- G + (discount ^ n) * V[ states[tau + n + 1] ]
      }
      
      if(states[tau + 1] != 1 & states[tau + 1] != terminal_state)
        
        V[ states[tau + 1] ] <<- V[ states[[tau + 1]] ] + alpha * (G - V[ states[tau + 1] ])
    }
    
    if(tau == episode_length)
      break
    
  }
  
  invisible(list("states" = states, "rewards" = rep(reward, length(states) - 1)))
}



residual_error <- function(num_episodes, terminal_state, n, ...){
  init_state_values(terminal_state)
  true_V <- seq(from = -1, to = 1, length.out = terminal_state)
  true_V <- true_V[2:(terminal_state-1)]
  rmse <- vector("double", num_episodes)
  for(i in seq_len(num_episodes)){
    episode(terminal_state, n = n, ...)
    rmse[[i]] <- sqrt(mean((V[2:(terminal_state - 1)] - true_V) ^ 2))
  }
  return(rmse)
}


# plotting figure 7.2

plot_fig7.2 <- function() {
  
  steps <- 2 ^ (0:9)
  alphas <- seq(from = 0, to = 1, by = 0.05)
  
  errors <- matrix(0, nrow = length(steps), ncol = length(alphas))
  
  
  ##----- tidy version (with purrr)

  for(run in 1:100){
    errors <- errors +
      cross2(steps, alphas) %>%
      transpose() %>%
      pmap_dbl(~sum(residual_error(n = .x, alpha = .y, num_episodes = 10, terminal_state = 21)))
  }
  
  ##----- simple version (with for loop)
  
  # errors <- matrix(0, nrow = length(steps), ncol = length(alphas))
  # for(run in 1:100){
  #   for(step in steps)
  #     for(alpha in alphas)
  #       errors[which(steps == step), which(alphas == alpha)] <-
  #         errors[which(steps == step), which(alphas == alpha)] +
  #         sum(residual_error(n = step, alpha = alpha, num_episodes = 10, terminal_state = 21))
  # }
  
  plot <- melt(errors / 1000) %>%
    mutate(Var2 = rep(alphas, each = length(steps)),
           Var1 = rep(steps, times = length(alphas))) %>% 
    ggplot(aes(x= Var2, y = value, group= Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(y = c(0.25, 0.55)) +
    labs(x = expression(alpha), y  = "average RMSE over 19 states\nand first 10 episodes", color = "nsteps")
  
  
  return(plot)
}


# plot_fig7.2()