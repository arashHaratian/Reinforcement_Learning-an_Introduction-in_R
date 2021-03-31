library(tidyverse)

resample <- function(x, ...)
  x[sample.int(length(x), ...)]

init_state_values <- function(terminal_state = 7){
  V <<- c(0, rep(0.5, terminal_state - 2), 1)
}

episode <- function(terminal_state = 7, method = "TD", batch = FALSE, alpha = 0.1) {
  if(!any(toupper(method) == c("TD", "MC")))
    stop("please select an valid method: 'TD' or 'MC'.\n")
  
  current_state <- terminal_state %/% 2 + 1   # middle state is the first state
  
  states <- c(current_state)
  
  while(current_state != terminal_state & current_state != 1){
    action <- resample(c(-1,1), 1) # next action on random
    
    current_state <- current_state + action
    reward <- 0
    states <- append(states, current_state)
    
    if(toupper(method) == "TD" & !batch){
      # updating the state value in each step
      V[current_state - action] <<- V[current_state - action] +
        alpha * (reward + V[current_state] - V[current_state - action])
    }
  }
  
  if(toupper(method) == "MC"){
    # Monte Carlo part
    reward <- ifelse(current_state == terminal_state, 1, 0)
    if(!batch){
      for(state in states)
        V[state] <<- V[state] + alpha * (reward - V[state])
    }
  }
  
  return(list("states" = states, "rewards" = rep(reward, length(states) - 1)))
}

#calculating
residual_error <- function(num_episodes, terminal_state, ...){
  init_state_values(terminal_state)
  true_V <- seq_len(terminal_state - 2) / (terminal_state - 1)
  rmse <- vector("double", num_episodes)
  for(i in seq_len(num_episodes)){
    episode(terminal_state, ...)
    rmse[[i]] <- sqrt(mean((V[2:(terminal_state - 1)] - true_V) ^ 2))
  }
  return(rmse)
}


batch_training <- function(num_episodes, terminal_state, method, alpha){
  init_state_values(terminal_state)
  
  batch_states <- list()
  batch_rewards <- list()
  errors <- vector("double", num_episodes)
  true_V <- seq_len(terminal_state - 2) / (terminal_state - 1)
  
  for(i in seq_len(num_episodes)){
    result <- episode(terminal_state, method = method, batch = TRUE)
    states <- result[["states"]]
    rewards <- result[["rewards"]]
    
    batch_states[[i]] <- states
    batch_rewards[[i]] <- rewards
    
    while(TRUE){
      update <- vector("double", length = terminal_state)
      
      for(j in seq_len(length(batch_rewards))){
        
        states <- batch_states[[j]]
        rewards <- batch_rewards[[j]]
        
        for(k in seq_len(length(rewards))) {
          if(toupper(method) == "TD"){
            update[[ states[[k]] ]] <- update[[ states[[k]] ]] + rewards[[k]] + V[states[[k + 1]]] - V[states[[k]]]
          } else{
            update[[ states[[k]] ]] <- update[[ states[[k]] ]] + rewards[[k]] - V[states[[k]]]
          }
        }
      }
      
      update <- update * alpha
      
      
      if(sum(abs(update)) < 1e-3)
        break
      
      V <<- V + update
    }
    
    errors[[i]] <- sqrt(mean((V[2:(terminal_state - 1)] - true_V) ^ 2))
  }
  
  return(errors)
}



# plotting figures

example6.2.1 <- function(with_terminal_states = F){
  init_state_values(7)
  
  
  result <- list(V) # first element is state value before running anything
  count <- 2 # second element and so one
  
  for(num_episode in 1:100){
    episode()
    if(num_episode %in% c(1, 10, 100)){
      result[[count]] <- V
      count <- count + 1
    }
  }
  
  names(result) <- paste(c(0, 1, 10, 100), "episodes")
  result[["true values"]] <- 0:6 / 6
  
  lines <- imap(result, ~geom_line(aes(y = .x, color = .y)))
  
  
  states <- data.frame("state" = 1:7)
  
  plot <- ggplot(states, aes(x = state)) +
    lines +
    labs(y = "estimated value", colour = "")
  
  if(with_terminal_states){
    plot <- ggplot(states, aes(x = state)) +
      lines +
      xlim(2, 6) +
      labs(y = "estimated value", colour = "")
  }
  return(plot)
}


example6.2.2 <- function(){
  
  TD_errors <- list()
  for(alpha in c(0.15, 0.1, 0.05)){
    error_episodes <- vector("double", 100)
    for(run in 1:100){
      error_episodes <- error_episodes + residual_error(num_episodes = 100,
                                                        terminal_state = 7,
                                                        alpha = alpha)
    }
    TD_errors[[as.character(alpha)]] <- error_episodes/100
  }
  
  TD_lines <- imap(TD_errors, ~geom_line(aes(y = .x, color = .y)))

  
  MC_errors <- list()
  for(alpha in c(0.01, 0.02, 0.03, 0.04)){
    error_episodes <- vector("double", 100)
    for(run in 1:100){
      error_episodes <- error_episodes + residual_error(num_episodes = 100,
                                                        terminal_state = 7,
                                                        method = "MC",
                                                        alpha = alpha)
    }
    MC_errors[[as.character(alpha)]] <- error_episodes/100
  }
  MC_lines <- imap(MC_errors, ~geom_line(aes(y = .x, color = .y), linetype = "longdash"))
  
  
  episodes <- data.frame("episodes" = 1:100)
  
  plot <- ggplot(episodes, aes(x = episodes)) +
    MC_lines +
    TD_lines +
    scale_color_discrete(labels = c(
      paste("MC alpha = ", c(0.01, 0.02, 0.03, 0.04)),
      paste("TD alpha = ", c(0.15, 0.1, 0.05)))) +
    labs("empirical RMSE , averaged over states", color = "alpha")
  
  return(plot)
}




plot_fig6.2 <- function(){
  
  error_episodes <- vector("double", 100)
  for(run in 1:100){
    error_episodes <- error_episodes + batch_training(method = 'TD',
                                                      num_episodes = 100,
                                                      terminal_state = 7,
                                                      alpha = 0.001)
  }
  errors <- data.frame("TD" = error_episodes/100)

  

  error_episodes <- vector("double", 100)
  for(run in 1:100){
    error_episodes <- error_episodes + batch_training(method = 'MC',
                                                      num_episodes = 100,
                                                      terminal_state = 7,
                                                      alpha = 0.001)
  }
  errors$MC <- error_episodes/100
  
  errors$episodes <- 1:100
  
  plot <- ggplot(errors, aes(x = episodes)) +
    geom_line(aes(y = TD, color = "TD")) +
    geom_line(aes(y = MC, color = "MC")) + 
    labs(x = "walks / episodes", y = "RMSE, averaged over states", color = "method", title = "batch training")
  
  return(plot)
}

example6.2.1()
example6.2.1(TRUE)
example6.2.2()
plot_fig6.2()

