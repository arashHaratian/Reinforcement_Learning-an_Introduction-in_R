library(tidyverse)
library(reshape2)
library(patchwork)
source("./Chapter10/tiles3.R")  # or copy the source code


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


mcar_max_position <- 0.5
mcar_min_position <- -1.2
mcar_max_velocity <- 0.07
mcar_min_velocity <- -0.07


init_weights <- function(size_val = 2048){
  w <<- vector("double", size_val)
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

episode_semi_gradient_n_step_sarsa <- function(IHT,
                                               assign_hash_function,
                                               n,
                                               alpha,
                                               epsilon = 0,
                                               discount = 1,
                                               num_of_tilings = 8){
  
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
  
  positions <- mcar_position
  velocities <- mcar_velocity
  rewards <- 0
  actions <- action
  
  
  time <- 0
  episode_length <- Inf
  
  while(TRUE){
    
    time <- time + 1
    
    if(time < episode_length){
      step <- mcar_step(mcar_position, mcar_velocity, action)
      mcar_position <- step[["mcar_position"]]
      mcar_velocity <- step[["mcar_velocity"]]
      
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
      
      positions <- append(positions, mcar_position)
      velocities <- append(velocities, mcar_velocity)
      rewards <- append(rewards, reward)
      actions <- append(actions, action)
      
      
      if(mcar_position == mcar_max_position)
        episode_length <- time
    }
    # semi gradient n-step sarsa, page 247
    tau <- time - n
    if(tau >= 0){
      range <- (tau + 1):(min(tau + n, episode_length) + 1) 
      G <- 0   # returns
      for(i in range){
        G <- G + (discount ^ (i - tau - 1)) * rewards[i]
      }
      
      if(tau + n <= episode_length){
        value <- Q(positions[tau + n + 1],
                   velocities[tau + n + 1],
                   actions[tau + n + 1],
                   IHT = IHT,
                   assign_hash_function = assign_hash_function)
        G <- G + (discount ^ n) * value
      }
      
      if(positions[tau + 1] != mcar_max_position){

        active_tiles <- get_tiles(positions[tau + 1],
                                  velocities[tau + 1],
                                  actions[tau + 1],
                                  IHT,
                                  assign_hash_function,
                                  num_of_tilings)
        
        delta <- (G - sum(w[active_tiles])) * (alpha / num_of_tilings)
        
        w[active_tiles] <<- w[active_tiles] + delta
      }
    }
    
    if(tau == episode_length - 1)
      break
  }
  
  return(time)
}



run <- function(num_of_episodes,n ,alpha, ...){
  init_weights()
  
  IHT <- initialize_IHT(2048)
  hash_table <- IHT[[1]]
  assign_hash_table <- IHT[[2]]
  
  
  steps <- vector("integer", num_of_episodes)
  
  for(episode in seq_len(num_of_episodes))
    steps[[episode]] <- episode_semi_gradient_n_step_sarsa(hash_table, assign_hash_table, n, alpha, ...)
  return(steps)
}



plot_fig10.1 <- function(target_episodes = c(1, 100, 1000, 9000), plot_heatmap = F){
  if(!plot_heatmap)
    library(plotly)
  
  init_weights()
  
  IHT <- initialize_IHT(2048)
  hash_table <- IHT[[1]]
  assign_hash_table <- IHT[[2]]
  
  max_episode <- max(target_episodes)
  
  # making the grid
  positions <- seq(mcar_min_position, mcar_max_position, length.out = 40)
  velocities <- seq(mcar_min_velocity, mcar_max_velocity, length.out = 40)
  grid <- expand_grid(positions, velocities, "actions" = 1:3)
  
  result <- list()
  
  
  for(episode in seq_len(max_episode)){
    episode_semi_gradient_n_step_sarsa(hash_table, assign_hash_table, 1, 0.2)
    
    if(episode %in% target_episodes){
      result[[as.character(episode)]] <- pmap_dbl(grid, ~(-max(Q(..1, ..2, ..3, hash_table, assign_hash_table))))
    }
  }
  if(plot_heatmap){
    plots <- imap(result, ~(ggplot(grid, aes(positions,  velocities)) +
                              geom_tile(aes( fill = .x)) +
                              labs(title = paste("step", .y), color = ""))
                  )
    return(wrap_plots(plots))
    
  } else {
    plots <- list()
    list_names <- names(result)
    for(episode in list_names){
      data <- grid %>% mutate("cost to go" = result[[episode]])
      plots[[episode]] <- plot_ly(data, x = ~positions, y = ~velocities, z = ~`cost to go`, scene = paste0("scene", episode)) %>%
        add_markers(size = 0.5) %>% 
        add_annotations(
          text = episode,
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
    }
    return(subplot(plots, nrows = NROW(2)))
  }
}



plot_fig10.2 <- function(runs = 10){
  alphas <- c(0.1, 0.2, 0.5)
  steps <- 0
  for(i in seq_len(runs)){
    cat("run ", i, '\n')
    steps <- steps + as.data.frame(map(alphas, ~run(500, 1, .x)))
  }
  
  names(steps) <- paste(alphas, "/ 8")
  
  df <- data.frame("episode" = 1:500)
  lines <- imap(steps, ~geom_line(aes(y = .x / runs, color = .y)))
  
  plot <- ggplot(df, aes(x = episode)) +
    lines +
    scale_y_log10() +
    labs(y = "Steps per episode(log scale)", color = "alpha value")
  
  return(plot)
}



plot_fig10.3 <- function(runs = 10){
  ns <- c(1, 8)
  steps <- 0
  for(i in seq_len(runs)){
    cat("run ", i, '\n')
    steps <- steps + as.data.frame(map(ns, ~run(500, .x, ifelse(.x == 1, 0.5, 0.3))))
  }
  
  names(steps) <- paste("n =", ns)
  
  df <- data.frame("episode" = 1:500)
  lines <- imap(steps, ~geom_line(aes(y = .x / runs, color = .y)))
  
  plot <- ggplot(df, aes(x = episode)) +
    lines +
    scale_y_log10() +
    labs(y = "Steps per episode(log scale)", color = "n-step value")
  
  return(plot)
}



plot_fig10.4 <- function(runs = 10){
    ns <- 2 ^ (0:4)
    alphas <- seq(from = 0.2, to = 1.8, by = 0.2)
    steps <- matrix(0, nrow = length(ns), ncol = length(alphas)) 
    
    for(i in seq_len(runs)){
      cat("run ", i, '\n')
      steps <- steps + 
        cross2(ns, alphas) %>%
        transpose() %>%
        pmap_dbl( ~if((.x == 8 & .y > 0.9) | (.x == 16 & .y > 0.75) | (.x == 4 & .y > 1.4)){
          # with these values, the algorithm did not converge.
          # assigning some value bigger than y axis upper limit (300) then it will be trimmed in plot
          20000 # (400 * 50)
        }else{
          sum(run(50, n = .x, alpha = .y))
        })
    }

    
    plot <- melt(steps / (runs * 50)) %>%
      mutate(Var2 = rep(alphas, each = length(ns)),
             Var1 = rep(ns, times = length(alphas))) %>%
      ggplot(aes(x = Var2, y = value, group = Var1, color = as.factor(Var1))) +
      geom_line() +
      coord_cartesian(ylim = c(220, 300)) +
      labs(x = expression(paste(alpha, " x number of tilings (8)")),
           y = "steps per episode\naveraged over first 50 episodes", color = "nsteps")
    
    return(plot)
}






plot_fig10.1()
plot_fig10.2(10)
plot_fig10.3(10)
plot_fig10.4(3)


