library(tidyverse)
library(reshape2)



resample <- function(x, ...)
  x[sample.int(length(x), ...)]


init_parameters <- function(terminal_state = 21) {
  w <<- rep(0, terminal_state)
  eligibility <<- vector("integer", terminal_state)
}



episode <- function(alpha,
                    lambda,
                    learning_function,
                    terminal_state = 21) {
  
    current_state <- terminal_state %/% 2 + 1
    reward <- 0
    
    last_state <- current_state
    old_state_value <- 0
    
    while (current_state != terminal_state & current_state != 1) {
      current_state <- current_state + resample(c(-1, 1), 1) # next action on random
      
      if (current_state == terminal_state) {
        reward <- 1
      } else if (current_state == 1) {
        reward <- -1
      }
      
      current_state_value <- w[[current_state]]
      
      learning_function(alpha,
                        lambda,
                        reward,
                        current_state_value,
                        last_state,
                        old_state_value)
      
      last_state <- current_state
      old_state_value <- current_state_value
    }
  }



TD_lambda <- function(alpha,
                      lambda,
                      reward,
                      current_state_value,
                      last_state,
                      ...) {
  eligibility <<- eligibility * lambda
  eligibility[[last_state]] <<- eligibility[[last_state]] + 1
  delta <- reward + current_state_value - w[[last_state]]
  delta <- delta * alpha
  w <<- w + delta * eligibility
}



true_online_TD_lambda <- function(alpha,
                                  lambda,
                                  reward,
                                  current_state_value,
                                  last_state,
                                  old_state_value) {
  
  
  last_state_value <- w[[last_state]]
  
  dutch <- 1 - alpha * lambda * eligibility[[last_state]]
  eligibility <<- eligibility * lambda
  eligibility[[last_state]] <<- eligibility[[last_state]] + dutch
  delta <- reward + current_state_value - last_state_value
  w <<- w + alpha * (delta +  last_state_value - old_state_value) * eligibility
  w[[last_state]] <<- w[[last_state]] - alpha * (last_state_value - old_state_value)
}




residual_error <- function(num_episodes, terminal_state, ...) {
  init_parameters(terminal_state)
  true_V <- seq(from = -1, to = 1, length.out = terminal_state)
  true_V <- true_V[2:(terminal_state - 1)]
  rmse <- vector("double", num_episodes)
  for (i in seq_len(num_episodes)) {
    episode(terminal_state = terminal_state, ...)
    rmse[[i]] <- sqrt(mean((w[2:(terminal_state - 1)] - true_V) ^ 2))
  }
  return(rmse)
}





plot_fig12.6 <- function() {


  lambdas <- c(0, 0.4, 0.8, 0.9, 0.95, 0.975, 0.99, 1)
  alphas <- seq(from = 0, to = 1, by = 0.05)

  errors <- matrix(0, nrow = length(lambdas), ncol = length(alphas))


  ##----- tidy version (with purrr)

  for(run in 1:50){
    errors <- errors +
      cross2(lambdas, alphas) %>%
      transpose() %>%
      pmap_dbl(~sum(residual_error(10, 21, alpha = .y, lambda = .x, learning_function = TD_lambda)))
  }

  plot <- melt(errors / 500) %>%
    mutate(Var2 = rep(alphas, each = length(lambdas)),
           Var1 = rep(lambdas, times = length(alphas))) %>%
    ggplot(aes(x= Var2, y = value, group = Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(y = c(0.25, 0.55)) +
    labs(title = "TD(lambda)",
         x = expression(alpha),
         y  = "RMSE over the first 10 episodes",
         color = "lambdas")


  return(plot)
}



plot_fig12.8 <- function() {


  lambdas <- c(0, 0.4, 0.8, 0.9, 0.95, 0.975, 0.99, 1)
  alphas <- seq(from = 0, to = 1, length.out = 21)

  errors <- matrix(0, nrow = length(lambdas), ncol = length(alphas))


  ##----- tidy version (with purrr)

  for(run in 1:50){
    errors <- errors +
      cross2(lambdas, alphas) %>%
      transpose() %>%
      pmap_dbl(~sum(residual_error(10,
                                   21,
                                   alpha = .y,
                                   lambda = .x,
                                   learning_function = true_online_TD_lambda)))
  }

  plot <- melt(errors / 500) %>%
    mutate(Var2 = rep(alphas, each = length(lambdas)),
           Var1 = rep(lambdas, times = length(alphas))) %>%
    ggplot(aes(x= Var2, y = value, group = Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(y = c(0.25, 0.55)) +
    labs(title = "online lambda-return algorithm",
         x = expression(alpha),
         y  = "RMSE over the first 10 episodes",
         color = "lambdas")


  return(plot)
}



plot_fig12.6()
plot_fig12.8()