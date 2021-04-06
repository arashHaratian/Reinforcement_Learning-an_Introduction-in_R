library(tidyverse)
library(reshape2)
library(patchwork)

resample <- function(x, ...)
  x[sample.int(length(x), ...)]


init_state_values <- function(value_function_type = c("state_aggregation", "polynomial_bases", "fourier_bases"),
                              num_of_groups = 10,
                              order){
  if((!(value_function_type %in% c("state_aggregation", "polynomial_bases", "fourier_bases"))) |
     length(value_function_type) != 1)
    stop("wrong 'value_function_type'\n choose one of: 'state_aggregation', 'polynomial_bases', 'fourier_bases'")
  
  
  if(value_function_type == "state_aggregation"){
    V <<- rep(0, num_of_groups)
  } else{
    # weights and bias(constant)
    weights_vector <<- vector("double", order + 1)
  }
}

get_value <- function(value_function_type = c("polynomial_bases", "fourier_bases"),
                      state,
                      order,
                      terminal_state = 1002) {
  
  if((!(value_function_type %in% c("polynomial_bases", "fourier_bases"))) |
     length(value_function_type) != 1)
    stop("wrong 'value_function_type'\n choose one of: 'polynomial_bases' or 'fourier_bases'")
  
  state <- state / (terminal_state - 2)
  
  if(value_function_type == "polynomial_bases"){
    feature <- state ^ (0:order)
  } else{
    feature <- cos(0:order * pi * state)
  }
  return(sum(weights_vector * feature)) # returning dot product of weights and features
}



set_value <- function(value_function_type = c("polynomial_bases", "fourier_bases"),
                      state,
                      order,
                      delta,
                      terminal_state = 1002) {
  
  if((!(value_function_type %in% c("polynomial_bases", "fourier_bases"))) |
     length(value_function_type) != 1)
    stop("wrong 'value_function_type'\n choose one of: 'polynomial_bases' or 'fourier_bases'")
  
  state <- state / (terminal_state - 2)
  
  if(value_function_type == "polynomial_bases"){
    derivative <- state ^ (0:order)
  } else{
    derivative <- cos(0:order * pi * state)
  }
  weights_vector <<- weights_vector + (delta * derivative)
  invisible(weights_vector)
}

episode_gradient_MC <- function(value_function_type = c("state_aggregation", "polynomial_bases", "fourier_bases"),
                                terminal_state = 1002,
                                group_size = 100,
                                max_jump = 100,
                                alpha = 0.1,
                                with_distribution = F,
                                order){
  
  current_state <- (terminal_state - 1) %/% 2  # middle state is the first state
  states <- c(current_state)
  
  while(TRUE){ 
    action <- resample(c(-max_jump:-1, 1:max_jump), 1) 
    
    current_state <- current_state + action 
    reward <- 0
    
    if(current_state >= terminal_state){   # stay in range of states and set reward
      current_state <- terminal_state
      reward <- 1                  # reward for ending on the left
      break
    } else if(current_state <= 1){
      current_state <- 1
      reward <- -1                 # reward for ending on the right
      break
    }
    
    states <- append(states, current_state)  
  }
  # gradient update
  for(state in states){
    group <- (state %/% group_size) + 1
    if(group < 11){
      if(value_function_type == "state_aggregation"){
        V[group] <<- V[group] + alpha * (reward - V[group])
      } else{
        delta <- alpha * (reward - get_value(value_function_type, state, order, terminal_state))
        set_value(value_function_type, state, order, delta, terminal_state)
      }
    }
    if(with_distribution)
      distribution[state - 1] <<- distribution[state - 1] + 1
  }
}




episode_semi_gradient_TD <- function(terminal_state = 1002,
                                     group_size = 100,
                                     max_jump = 100,
                                     alpha = 0.1,
                                     n = 1,
                                     discount = 1) {
  
  current_state <- (terminal_state - 1) %/% 2  # middle state is the first state
  
  states <- c(current_state)
  rewards <- c(0)
  
  time <- 0
  episode_length <- Inf
  
  while(TRUE){
    
    time <- time + 1
    
    if(time < episode_length){
      
      action <- resample(c(-max_jump:-1, 1:max_jump), 1) # next jump on random
      
      current_state <- current_state + action
      
      reward <- 0
      
      if(current_state >= terminal_state){   # stay in range of states and set reward
        current_state <- terminal_state
        reward <- 1                  # reward for ending on the left
      } else if(current_state <= 1){
        current_state <- 1
        reward <- -1                 # reward for ending on the right
      }
      
      
      # storing new state and new reward
      states <- append(states, current_state)
      rewards <- append(rewards, reward)
      
      
      if(current_state == terminal_state | current_state == 1)
        episode_length <- time
      
    }
    
    # n-step semi gradient TD, page 209
    tau <- time - n
    
    if(tau >= 0){
      range <- (tau + 1):(min(tau + n, episode_length) + 1) 
      G <- 0   # returns
      for(i in range){
        G <- G + (discount ^ (i - tau - 1)) * rewards[i]
      }
      
      if(tau + n <= episode_length){
        if(states[tau + n + 1] == 1 | states[tau + n + 1] == terminal_state){ # check if the state is terminal
          state_value <- 0                                                    # and get its value
        } else{
          group <- ((states[tau + n + 1] - 2) %/% group_size) + 1
          state_value <- V[group]
        }
        G <- G + (discount ^ n) * state_value
      }
      
      if(states[tau + 1] != 1 & states[tau + 1] != terminal_state){
        group <- ((states[tau + 1] - 2) %/% group_size) + 1
        V[group] <<- V[group] + alpha * (G - V[group])
      }
    }
    
    if(tau == episode_length - 1)
      break
    
  }
  
  invisible(list("states" = states, "rewards" = rep(reward, length(states) - 1)))
}




true_state_values <- function(terminal_state = 1002,
                              max_jump = 100){
  
  # compute true value states using Dynamic programming
  true_value <- seq(from = -1, to = 1, length.out = terminal_state)
  while(TRUE){
    old_value <- true_value
    for(state in 2:(terminal_state - 1)){
      true_value[state] <- 0
      for(jump in c(-max_jump:-1, 1:max_jump))
      {
        new_state <- state + jump
        if(new_state <= 1){
          new_state <- 1
        }else if (new_state >= terminal_state){
          new_state <- terminal_state
        }
        true_value[state] = true_value[state] + (1 / (2 * max_jump) * true_value[new_state])
      }
    }
    error <- sum(abs(old_value - true_value))
    if(error < 0.01)
      break
  }
  return(true_value)
}


residual_error_semi_gradient_TD <-function(true_value,
                                           num_episodes,
                                           num_of_groups,
                                           terminal_state,
                                           n,
                                           alpha,
                                           ...) {
  init_state_values("state_aggregation", num_of_groups)
  true_value <- true_value[2:(terminal_state - 1)]
  rmse <- vector("double", num_episodes)
  group_size <- (terminal_state - 2) / num_of_groups #computing group size
  for(i in seq_len(num_episodes)){
    episode_semi_gradient_TD(terminal_state = terminal_state, n = n, alpha = alpha, group_size = group_size, ...)
    estimated_state_values <- rep(V, each = group_size)
    rmse[[i]] <- sqrt(mean((estimated_state_values - true_value) ^ 2))
  }
  return(rmse)
}


residual_error_gradient_MC <- function(value_function_type,
                                       true_value,
                                       num_episodes,
                                       terminal_state,
                                       alpha,
                                       num_of_groups,
                                       order,
                                       ...) {
  
  init_state_values(value_function_type, num_of_groups, order)
  
  true_value <- true_value[2:(terminal_state - 1)] # remove terminal state values
  
  if(value_function_type == "state_aggregation")
    group_size <- (terminal_state - 2) / num_of_groups #computing group size
  
  rmse <- vector("double", num_episodes)
  
  for(i in seq_len(num_episodes)){
    episode_gradient_MC(value_function_type, terminal_state, alpha = alpha, order = order, ...)
    if(value_function_type == "state_aggregation") {
      
      estimated_state_values <- rep(V, each = group_size)
    } else{
      estimated_state_values <- seq_len(terminal_state)
      for(state in seq_len(terminal_state)){
        estimated_state_values[state] <- get_value(value_function_type, state, order, terminal_state)
      }
      estimated_state_values <- estimated_state_values[2:(terminal_state - 1)]
    }
    
    rmse[[i]] <- sqrt(mean((estimated_state_values - true_value) ^ 2))
  }
  return(rmse)
}



# plotting figures

plot_fig9.1 <- function(){
  
  init_state_values("state_aggregation", 10)
  distribution <<- vector("double", length = 1000)
  
  # compute true value states or use the global one
  # true_value <- true_state_values()
  true_value <- true_value[2:1001]
  
  for(i in 1:1e5)
    episode_gradient_MC(value_function_type = "state_aggregation", alpha = 2e-5, with_distribution = T)
  
  distribution <- distribution/sum(distribution)
  
  df <- data.frame("state" = 1:1000, "value" = rep(V, each = 100), true_value, distribution)
  
  plot1 <- df %>% 
    ggplot(aes(x = state)) +
    geom_line(aes(y = value, color = "approximate MC value")) +
    geom_line(aes(y = true_value, color = "true value"))
  
  plot2 <- df %>% 
    ggplot(aes(x = state)) +
    geom_freqpoly(aes(y = distribution), stat = "identity") +
    labs(title = "state distribution")
  
  wrap_plots(plot1, plot2)
}



plot_fig9.2.1 <- function() {
  
  # compute true value states or use the global one
  # true_value <- true_state_values()
  true_value <- true_value[2:1001]
  
  init_state_values("state_aggregation", 10)
  for(i in 1:1e5)
    episode_semi_gradient_TD(alpha = 2e-4)
  
  df <- data.frame("state" = 1:1000, "value" = rep(V, each = 100), true_value)
  
  plot <- df %>% 
    ggplot(aes(x = state)) +
    geom_line(aes(y = value, color = "approximate TD value")) +
    geom_line(aes(y = true_value, color = "true value"))
  
  return(plot)
}



plot_fig9.2.2 <- function() {
  
  # compute true value states or use the global one
  # true_value <- true_state_values()
  
  steps <- 2 ^ (0:9)
  alphas <- seq(from = 0, to = 1, by = 0.1) # or use by = 0.05
  errors <- matrix(0, nrow = length(steps), ncol = length(alphas))
  
  for(run in 1:100){
    errors <- errors +
      cross2(steps, alphas) %>%
      transpose() %>%
      pmap_dbl( ~ sum(
        residual_error_semi_gradient_TD(
          n = .x,
          alpha = .y,
          num_of_groups = 20,
          num_episodes = 10,
          terminal_state = 1002,
          true_value = true_value)
      ))
  }
  
  plot <- melt(errors / 1000) %>%
    mutate(Var2 = rep(alphas, each = length(steps)),
           Var1 = rep(steps, times = length(alphas))) %>% 
    ggplot(aes(x= Var2, y = value, group= Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(y = c(0.25, 0.55)) +
    labs(x = expression(alpha), y  = "average RMSE over 1000 states\nand first 10 episodes", color = "nsteps")
  
  
  return(plot)
}



plot_fig9.5 <- function(runs = 30){
  
  orders <- c(5, 10, 20)
  
  # compute true value states or use the global one
  # true_value <- true_state_values()
  
  error_poly <- list()
  
  for(order in orders){
    error <- 0
    for(run in seq_len(runs)){
      init_state_values("polynomial_bases", order = order)
      error <- error + residual_error_gradient_MC("polynomial_bases",
                                                  terminal_state = 1002,
                                                  order = order,
                                                  true_value = true_value ,
                                                  num_episodes = 5000,
                                                  alpha =  0.0001)
    }
    error_poly[[as.character(order)]] <- error / runs
  }
  
  
  error_fourier <- list()
  for(order in orders){
    error <- 0
    for(run in seq_len(runs)){
      init_state_values("fourier_bases", order = order)
      error <- error + residual_error_gradient_MC("fourier_bases",
                                                  terminal_state = 1002,
                                                  order = order,
                                                  true_value = true_value ,
                                                  num_episodes = 5000,
                                                  alpha =  5e-5)
    }
    error_fourier[[as.character(order)]] <- error / runs
  }
  
  
  episodes <- data.frame("episodes" = 1:5000)
  
  lines_poly <- error_poly %>% 
    imap(~geom_line(aes(y = .x/runs, color = paste0("polynomial, order: ", .y))))
  
  lines_fourier <- error_fourier %>% 
    imap(~geom_line(aes(y = .x/runs, color = paste0("fourier, order: ", .y))))
  
  plot <- ggplot(episodes, aes(x = episodes)) +
    lines_poly +
    lines_fourier +
    labs(y = "RMSVE", color = "")
  
  return(plot)
}






true_value <- true_state_values()

plot_fig9.1()
plot_fig9.2.1()
plot_fig9.2.2()
plot_fig9.5(2)
