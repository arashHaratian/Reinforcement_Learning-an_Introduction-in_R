library(tidyverse)
library(reshape2)



resample <- function(x, ...)
  x[sample.int(length(x), ...)]


init_weights <- function(terminal_state = 21){
  w <<- rep(0, terminal_state)
}



episode <- function(alpha, lambda, terminal_state = 21){
  current_state <- terminal_state %/% 2 + 1
  reward <- 0
  states <- current_state  # the trajectory
  
  while(current_state != terminal_state & current_state != 1){
    
    current_state <- current_state + resample(c(-1, 1), 1) # next action on random
    states <- append(states, current_state)
    
    if(current_state == terminal_state){
      reward <- 1
    } else if(current_state == 1){
      reward <- -1
    }
  }
  
  offline_learning(states, alpha, lambda, reward)
}



offline_learning <- function(states, alpha, lambda, reward) {
  episode_length <- length(states) - 1
  for(time in seq_len(episode_length)){
    state <- states[[time]]
    
    returns <- 0
    power_of_lambda <- 1
    
    for(n in seq_len(episode_length - time + 1)){
      if(n == 1)
        next
      
      end_time <- min(time + n, episode_length) - 1
      target <- w[[ states[[end_time]] ]]
      
      if(end_time == episode_length)
        target <- target + reward
      
      returns <- returns + power_of_lambda * target
      power_of_lambda <- power_of_lambda * lambda
      
      if(power_of_lambda < 0.001)
        break
    }
    
    returns <- returns * (1 - lambda)
    
    if(power_of_lambda >= 0.001)
      returns <- returns + power_of_lambda * reward
    
    # returns <- lambda_return_from_time(time, episode_length, lambda, reward, states)
    
    delta <- returns - w[[state]]
    delta <- delta * alpha
    w[[state]] <<- w[[state]] + delta
  }
}



residual_error <- function(num_episodes, terminal_state, ...){
  init_weights(terminal_state)
  true_V <- seq(from = -1, to = 1, length.out = terminal_state)
  true_V <- true_V[2:(terminal_state - 1)]
  rmse <- vector("double", num_episodes)
  for(i in seq_len(num_episodes)){
    episode(terminal_state, ...)
    rmse[[i]] <- sqrt(mean((w[2:(terminal_state - 1)] - true_V) ^ 2))
  }
  return(rmse)
}







plot_fig12.3 <- function() {
  
  
  lambdas <- c(0, 0.4, 0.8, 0.9, 0.95, 0.975, 0.99, 1)
  alphas <- seq(from = 0, to = 1, length.out = 15)

  errors <- matrix(0, nrow = length(lambdas), ncol = length(alphas))


  ##----- tidy version (with purrr)

  for(run in 1:50){
    errors <- errors +
      cross2(lambdas, alphas) %>%
      transpose() %>%
      pmap_dbl(~sum(residual_error(10, 21, alpha = .y, lambda = .x)))
  }

  plot <- melt(errors / 500) %>%
    mutate(Var2 = rep(alphas, each = length(lambdas)),
           Var1 = rep(lambdas, times = length(alphas))) %>%
    ggplot(aes(x= Var2, y = value, group = Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(y = c(0.25, 0.55)) +
    labs(title = "offline lambda-return algorithm",
         x = expression(alpha),
         y  = "RMSE over the first 10 episodes",
         color = "lambdas")


  return(plot)
}


# plot_fig12.3()
