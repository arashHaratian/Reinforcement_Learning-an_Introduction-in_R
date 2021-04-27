library(tidyverse)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]

states <- 1:3  # (B, A, terminal)

initial_action_values <- function(){
  Q1 <<- Q2 <<- list("B" = vector("double", 12),  #  10 actions in B
                   "A" = vector("double", 2),   # (left, right) in A
                   "terminal" = 0)
}

 
move <- function(state, Q1, Q2 = NULL) {

  #selecting action
  if(runif(1, 0, 1) < 0.1){
    # exploring
    if(state == 2){

      # possible actions for state A
      action <- resample(1:2, 1)
      new_state <- state + ifelse(action == 1, -1, 1)
      reward <- 0
    } else {
      # possible actions for state B
      action <- resample(1:10, 1)
      reward <- rnorm(1, -0.1, 1)
      new_state <- 3
    }

  } else{
    # greedy action
    if(state == 2){

      # best action for state A
      if(!is.null(Q2)){
        vals <- Q1[[state]] + Q2[[state]]
        action <- resample(which(vals == max(vals)), 1)  # max val with tie break
        new_state <- state + ifelse(action == 1, -1, 1)  #index to action
        reward <- 0
      } else {
        vals <- Q1[[state]]
        action <-resample(which(vals == max(vals)), 1)   # max val with tie break
        new_state <- state + ifelse(action == 1, -1, 1)  #index to action
        reward <- 0
      }
    } else {

      # best action for state B
      if(!is.null(Q2)){
        vals <- Q1[[state]] + Q2[[state]]
        action <- resample(which(vals == max(vals)), 1)  # max val with tie break
        new_state <- 3
        reward <- rnorm(1, -0.1, 1)
      } else {
        vals <- Q1[[state]]
        action <-resample(which(vals == max(vals)), 1)  # max val with tie break
        new_state <- 3
        reward <- rnorm(1, -0.1, 1)
      }
    }

  }
  return(list("reward" = reward, "new_state" = new_state, "action" = action))
}
 


Q_learning <- function(Q1, Q2 = NULL, alpha = 0.1, discount = 1){
  current_state <- 2  # first state is A (index 2 in "state" vector)
  left_count <- 0
  
  while(current_state != 3){
    result <- move(current_state, Q1, Q2)
    reward <- result[["reward"]]
    new_state <- result[["new_state"]]
    action <- result[["action"]]
    
    if(current_state == 2 & action == 1)  # moving left from A
      left_count <- left_count + 1
    
    # update the action values
    if(is.null(Q2)){
      #single Q
      Q1[[current_state]][[action]] <<- Q1[[current_state]][[action]] +
        alpha * (reward + discount * max(Q1[[new_state]]) - Q1[[current_state]][[action]])  
      
    } else{
      # double Q
      if(runif(1, 0, 1) < 0.5)
      {
        Q1[[current_state]][[action]] <<- Q1[[current_state]][[action]] +
          alpha * (reward + discount * Q2[[new_state]][[ which.max(Q1[[new_state]]) ]] - Q1[[current_state]][[action]])  
        
      } else {
        Q2[[current_state]][[action]] <<- Q2[[current_state]][[action]] +
          alpha * (reward + discount * Q1[[new_state]][[ which.max(Q2[[new_state]]) ]] - Q2[[current_state]][[action]])
      }
      
    }
    current_state <- new_state
  }
  return(left_count)
}



# plotting figure 6.5

plot_fig6.5 <- function(){
  
  left_single_Q <- vector("double", 300)
  for(run in 1:10000){
    initial_action_values()
    left_count <- vector("double", 300)
    for(episode in 1:300){
      left_count[[episode]] <- Q_learning(Q1)
    }
    left_single_Q <- left_single_Q + left_count
  }
  counts <- data.frame("single_Q" = left_single_Q / 10000)  
  
  
  left_double_Q <- vector("double", 300)
  for(run in 1:10000){
    initial_action_values()
    left_count <- vector("double", 300)
    for(episode in 1:300){
      left_count[[episode]] <- Q_learning(Q1, Q2)
    }
    left_double_Q <- left_double_Q + left_count
  }
  counts$double_Q <- left_double_Q / 10000
  counts$optimal <- 0.05
  counts$episodes <- 1:300
  
  plot <- ggplot(counts, aes(x = episodes)) +
    geom_line(aes(y = single_Q, color = "Q-learning")) +
    geom_line(aes(y = double_Q, color = "double Q-learning")) +
    geom_line(aes(y = optimal, color = "optimal"), linetype = "longdash") +
    labs(y = "(%) left actions from A", color = "")
  
  return(plot)
  
}


# plot_fig6.5()
