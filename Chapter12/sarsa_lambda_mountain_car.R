library(tidyverse)
library(reshape2)
source("./Chapter12/tiles3.R")  # or copy the source code


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


mcar_max_position <- 0.5
mcar_min_position <- -1.2
mcar_max_velocity <- 0.07
mcar_min_velocity <- -0.07


init_parameters <- function(size_val = 2048){
  w <<- vector("double", size_val)
  trace_of_tiles <<- vector("double", size_val)
}


mcar_step <- function(mcar_position, mcar_velocity, action){
  
  mcar_velocity <- mcar_velocity + (action - 2) * 0.001 + -0.0025 * cos(3 * mcar_position)
  mcar_velocity <- min(max(mcar_min_velocity, mcar_velocity), mcar_max_velocity)
  
  mcar_position <- mcar_position + mcar_velocity
  mcar_position <- min(max(mcar_min_position, mcar_position), mcar_max_position)
  
  if (mcar_position == mcar_min_position & mcar_velocity < 0)
    mcar_velocity <- 0
  
  return(list("mcar_position" = mcar_position, "mcar_velocity" = mcar_velocity))
}


Q <- function(mcar_position, mcar_velocity, action, ...){  
  
  if(mcar_position == mcar_max_position)
    return(0)
  
  active_tiles <- get_tiles(mcar_position, mcar_velocity, action, ...)
  
  return(sum(w[active_tiles]))
}


get_tiles <- function(mcar_position,
                      mcar_velocity,
                      action,
                      IHT,
                      assign_hash_function,
                      num_of_tilings = 8){
  
  position_scale <- num_of_tilings / (mcar_max_position - mcar_min_position)
  velocity_scale <- num_of_tilings / (mcar_max_velocity - mcar_min_velocity)
  
  mcar_position <- position_scale * mcar_position
  mcar_velocity <- velocity_scale * mcar_velocity
  
  active_tiles <- tiles(IHT,
                        assign_hash_function,
                        num_of_tilings,
                        c(mcar_position, mcar_velocity),
                        action)
  return(active_tiles)
}



accumulating_trace <- function(active_tiles, lambda, discount, ...){
  trace_of_tiles <<- trace_of_tiles * lambda * discount
  trace_of_tiles[active_tiles] <<- trace_of_tiles[active_tiles] + 1
}



replacing_trace <- function(active_tiles, lambda, discount, ...){
  active <- seq_along(trace_of_tiles) %in% active_tiles
  trace_of_tiles[active] <<- 1
  trace_of_tiles[!active] <<- trace_of_tiles[!active] * lambda * discount
}



replacing_trace_and_cleaning <- function(active_tiles, lambda, discount, other_actions_traces, ...){
  active <- seq_along(trace_of_tiles) %in% active_tiles
  trace_of_tiles[!active] <<- trace_of_tiles[!active] * lambda * discount
  trace_of_tiles[other_actions_traces] <<- 0
  trace_of_tiles[active] <<- 1
  
}



dutch_trace <- function(active_tiles, lambda, discount, alpha, ...){
  coef <- 1 - alpha * discount *lambda * sum(trace_of_tiles[active_tiles])
  trace_of_tiles <<- trace_of_tiles * lambda * discount
  trace_of_tiles[active_tiles] <<- trace_of_tiles[active_tiles] + coef
  
}




episode_sarsa_lambda <- function(IHT,
                         assign_hash_function,
                         alpha,
                         lambda,
                         trace_update_rule,
                         epsilon = 0,
                         discount = 1,
                         num_of_tilings = 8,
                         max_steps = 5000){
  
  alpha <- (alpha / num_of_tilings)
  
  # initial the position and velocity
  
  mcar_position <- -0.6 + runif(1, min = 0, max = 0.2)
  mcar_velocity <- 0
  
  # choosing action
  if(runif(1) < epsilon){
    action <- resample(1:3, 1)
  } else{
    values <- map_dbl(1:3, ~Q(mcar_position,
                              mcar_velocity,
                              .x,
                              IHT = IHT,
                              assign_hash_function = assign_hash_function))
    
    action <- resample(which(values == max(values)), 1)
  }
  
  # reward is always -1
  reward <- -1
  
  step_count <- 0
  
  while(TRUE){
    step_count <- step_count + 1
    
    step <- mcar_step(mcar_position, mcar_velocity, action)
    new_mcar_position <- step[["mcar_position"]]
    new_mcar_velocity <- step[["mcar_velocity"]]
    
    if(runif(1) < epsilon){
      new_action <- resample(1:3, 1)
    } else{
      values <- map_dbl(1:3, ~Q(new_mcar_position,
                                new_mcar_velocity,
                                .x,
                                IHT = IHT,
                                assign_hash_function = assign_hash_function))
      
      new_action <- resample(which(values == max(values)), 1)
    }
    
    target <- reward + discount * Q(new_mcar_position, new_mcar_velocity, new_action,
                                    IHT = IHT,
                                    assign_hash_function = assign_hash_function)
    
    # learning part
    active_tiles <- get_tiles(mcar_position,
                              mcar_velocity,
                              action,
                              IHT,
                              assign_hash_function,
                              num_of_tilings)
    
    delta <- target - sum(w[active_tiles])

    
    if(identical(trace_update_rule, replacing_trace_and_cleaning)){

      other_actions_traces <- vector("integer")
      for(a in 1:3){
        if(a != action)
          other_actions_traces <- append(other_actions_traces,
                                         get_tiles(mcar_position,
                                                   mcar_velocity,
                                                   a,
                                                   IHT,
                                                   assign_hash_function,
                                                   num_of_tilings))
      }
    }
    
    trace_update_rule(active_tiles,
                      lambda,
                      discount,
                      alpha = alpha,
                      other_actions_traces = other_actions_traces)
    
    w <<- w + alpha * delta * trace_of_tiles
    
    mcar_position <- new_mcar_position
    mcar_velocity <- new_mcar_velocity
    action <- new_action
    
    
    if(mcar_position == mcar_max_position | step_count >= max_steps)
      break

  }
  
  return(step_count)
}





run <- function(num_of_episodes,alpha, lambda, trace_update_rule, ...){
  init_parameters()
  
  IHT <- initialize_IHT(2048)
  hash_table <- IHT[[1]]
  assign_hash_table <- IHT[[2]]
  
  
  steps <- vector("integer", num_of_episodes)
  
  for(episode in seq_len(num_of_episodes))
    steps[[episode]] <-episode_sarsa_lambda(hash_table,
                                            assign_hash_table,
                                            alpha,
                                            lambda,
                                            trace_update_rule,
                                            ...)
  return(steps)
}





# plotting figures 12.10 and 12.11

plot_fig12.10 <- function(runs = 10){
  
  
  alphas <- seq(0.25, 1.75, by = 0.25)
  lambdas <- c(0.99, 0.96, 0.84, 0.68, 0)
  
  
  steps <- matrix(0, nrow = length(lambdas), ncol = length(alphas)) 
  for(i in seq_len(runs)){
    cat("run ", i, '\n')
    
    steps <- steps + 
      cross2(lambdas, alphas) %>%
      transpose() %>%
      pmap_dbl(~sum(run(50, lambda = .x, alpha = .y, trace_update_rule = replacing_trace)))
  }
  
  plot <- melt(steps / (runs * 50)) %>%
    mutate(Var2 = rep(alphas, each = length(lambdas)),
           Var1 = rep(lambdas, times = length(alphas))) %>%
    ggplot(aes(x = Var2, y = value, group = Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(ylim = c(180, 300)) +
    labs(title = "Sarsa(lambda) with replacing traces",
         x = expression(paste(alpha, " x number of tilings (8)")),
         y  = "steps per episode\naveraged over first 50 episodes", color = "lambda")
  
  return(plot)
}




plot_fig12.11 <- function(runs = 10){
  
  
  alphas <- seq(0.2, 2, by = 0.2)
  functions <- c(accumulating_trace, replacing_trace, replacing_trace_and_cleaning, dutch_trace)
  
  reward <- matrix(0, nrow = length(functions), ncol = length(alphas)) 
  for(i in seq_len(runs)){
    cat("run ", i, '\n')
    
    reward <- reward + 
      cross2(functions, alphas) %>%
      transpose() %>%
      pmap_dbl(~{
        if(identical(.x, accumulating_trace) & .y > 0.5){
          # maximum steps
          5000 * 20
        } else{
          sum(run(20, lambda = 0.9, alpha = .y, trace_update_rule = .x))
        }
      })
  }
  
  
  functions_names <- c("Sarsa(lambda) with accumulating traces",
                       "Sarsa(lambda) with replacing traces",
                       "Sarsa(lambda) with replacing traces\nand clearing the traces of other actions",
                       "true sarsa(lambda)")
  
  
  plot <- melt(-reward / (runs * 20)) %>%
    mutate(Var2 = rep(alphas, each = length(functions)),
           Var1 = rep(functions_names, times = length(alphas))) %>%
    ggplot(aes(x = Var2, y = value, group = Var1, color = as.factor(Var1))) +
    geom_line() +
    coord_cartesian(ylim = c(-550, -150)) +
    labs(x = expression(paste(alpha, " x number of tilings (8)")),
         y  = "rewards per episode\naveraged over first 2- episodes", color = "") +
    theme(legend.position = "bottom")
  
  return(plot)
}




plot_fig12.10()
plot_fig12.11()

