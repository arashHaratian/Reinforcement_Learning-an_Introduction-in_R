library(tidyverse)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]



init_parameters <- function(with_weights = FALSE){
  
  theta <<- c(-1.47, 1.47)
  
  if(with_weights)
    w <<- 0
}



step <- function(state, action){

  if(state %in% c(1, 3)){
    state <- state + action
    state <- max(state, 1)
  } else if(state == 2){
    state <- state - action
  }
  return(state)
}



pi <- function(state_features){
  
  preferences <- theta %*% state_features
  preferences <- preferences - max(preferences)
  probs <- exp(preferences) / sum(exp(preferences))
  
  if(min(probs) < 0.05){
    probs <- rep(1 - 0.05, length(probs))
    probs[which.min(probs)] <- 0.05
  }
  if(length(probs) != 2)
    browser()
  return(as.vector(probs))
}



select_action <- function(state_features){
  
  probs <- pi(state_features)
  action <- resample(c(-1, 1), 1, prob = probs)
  return(action)
}



reinforce <- function(alpha, discount = 1){
  
  state_features <- matrix(c(0, 1, 1, 0), nrow = 2)
  
  current_state <- 1
  reward <- -1
  
  rewards <- vector("double")
  actions <- vector("integer")
  
  
  while(TRUE){
    action <- select_action(state_features)
    current_state <- step(current_state, action)

    actions <- append(actions, action)
    
    if(current_state == 4){
      reward <- 0
      rewards <- append(rewards, reward)
      
      # learn theta
      
      G <- vector("double", length(rewards))
      G[length(G)] <- reward
      
      for(i in (length(G) - 1):1){
        G[i] <- discount * G[i + 1] + rewards[i]
      }
      
      discount_power <- 1
      
      for(i in seq_along(G)){
        j <- ifelse(actions[i] == 1, 2, 1)
        probs <- pi(state_features)
        gradient_ln_pi <- state_features[, j] - (state_features %*% probs)
        theta <<- theta + as.vector(alpha * discount_power * G[i] * gradient_ln_pi)
        
        discount_power <- discount_power * discount
      }
      
      break
    } 
    
    rewards <- append(rewards, reward)
  }
  return(sum(rewards))
}




reinforce_with_baseline <- function(alpha, alpha_w, discount = 1){
  
  state_features <- matrix(c(0, 1, 1, 0), nrow = 2)
  
  current_state <- 1
  reward <- -1
  
  rewards <- vector("double")
  actions <- vector("integer")
  
  
  while(TRUE){
    action <- select_action(state_features)
    current_state <- step(current_state, action)
    
    actions <- append(actions, action)
    
    if(current_state == 4){
      reward <- 0
      rewards <- append(rewards, reward)
      
      # learn theta
      
      G <- vector("double", length(rewards))
      G[length(G)] <- reward
      
      for(i in (length(G) - 1):1){
        G[i] <- discount * G[i + 1] + rewards[i]
      }
      
      discount_power <- 1
      
      for(i in seq_along(G)){
        w <<- w + alpha_w * discount_power * (G[i] - w)
        
        j <- ifelse(actions[i] == 1, 2, 1)
        probs <- pi(state_features)
        gradient_ln_pi <- state_features[, j] - (state_features %*% probs)
        theta <<- theta + as.vector(alpha * discount_power * (G[i] - w) * gradient_ln_pi)
        
        discount_power <- discount_power * discount
      }
      
      break
    } 
    
    rewards <- append(rewards, reward)
  }
  return(sum(rewards))
}




run <- function(method, num_of_episodes, alpha, with_weights = FALSE, ...){
  
  init_parameters(with_weights)
  
  rewards <- vector("integer", num_of_episodes)
  for(episode in seq_len(num_of_episodes)){
    rewards[episode] <- method(alpha, ...)
  }
  
  return(rewards)
}




# plotting figures


example13.1 <- function(){
  probs <- seq(0.01, 0.99, by = 0.01)
  true_value <- (2 * probs - 4) / (probs * (1 - probs))
  
  true_value_df <- as.data.frame(true_value) %>% 
    mutate(probs)
  
  optimal_point <- data.frame("probs" = probs[which.max(true_value)], "true_value" = c(max(true_value)))
  epsilon_left_point <- data.frame("probs" = 0.05, "true_value" = (2 * 0.05 - 4) / (0.05 * 0.95) )
  epsilon_right_point <- data.frame("probs" = 0.95, "true_value" = (2 * 0.95 - 4) / (0.95 * 0.05) )
  
  plot <- ggplot(true_value_df, aes(x = probs, y = true_value)) +
    geom_line() +
    geom_point(aes(color = "optimal optimal policy"), data = optimal_point, size = 2) +
    geom_point(aes(color = "epsilon-greedy left"), data = epsilon_left_point, size = 2) +
    geom_point(aes(color = "epsilon-greedy right"), data = epsilon_right_point, size = 2) +
    coord_cartesian(ylim = c(-100, 0)) +
    geom_text(aes(label = round(true_value, 2)), data = optimal_point, nudge_y = 5) +
    labs(x = "probability of right action",
         y = "value of the first state",
         color = "") +
    theme(legend.position = "bottom")
  
  return(plot)
}



plot_fig13.1 <- function(){
  alphas <- c(2e-3, 2e-4, 2e-5)
  
  rewards <- list("alpha = 2e-3" = 0,
                  "alpha = 2e-4" = 0,
                  "alpha = 2e-5" = 0)
  
  
  for(alpha in alphas){
    index <- which(alphas == alpha)
    for(i in 1:100){
      rewards[[index]] <- rewards[[index]] + run(reinforce, 1000, alpha)
    }
  }
  
  lines <- imap(rewards, ~geom_line(aes(y = .x/100, color = .y)))
  
  episodes <- data.frame("episode" = 1:1000)
  
  ggplot(episodes, aes(x = episode)) + 
    lines +
    geom_hline(yintercept = -11.66, linetype = "dashed") +
    labs(y = "total reward on episode", color = "")
  
}



plot_fig13.2 <- function(){
  
  functions <- c(reinforce, reinforce_with_baseline)
  
  rewards <- list("reinforce" = 0,
                  "reinforce_with_baseline" = 0)
  
  
  for(index in seq_along(functions)){
    if(index == 1){
      #reinforce
      for(i in 1:100){
        rewards[[index]] <- rewards[[index]] + run(functions[[index]], 1000, 2e-4)
      }
    } else{
      #reinforce_with_baseline
      for(i in 1:100){
        rewards[[index]] <- rewards[[index]] + run(functions[[index]], 1000, 2e-3, TRUE, alpha_w = 2e-2)
      }
    }
  }
  
  lines <- imap(rewards, ~geom_line(aes(y = .x/100, color = .y)))
  
  episodes <- data.frame("episode" = 1:1000)
  
  plot <- ggplot(episodes, aes(x = episode)) + 
    lines +
    geom_hline(yintercept = -11.66, linetype = "dashed") +
    labs(y = "total reward on episode", color = "")
  
  return(plot)
}





example13.1()
plot_fig13.1()
plot_fig13.2()
