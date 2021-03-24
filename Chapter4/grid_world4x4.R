library(tidyverse)
library(reshape2)


actions <- list(
  "U" = c(-1, 0),
  "D" = c(1, 0),
  "L" = c(0, -1),
  "R" = c(0, 1)
)


cols <- 4
rows <- 4



is_off_grid <- function(state, action) {
  new_state <- next_state(state, action)
  if(new_state[["x"]] > cols | new_state[["y"]] > rows | new_state[["x"]] < 1 | new_state[["y"]] < 1)
    return(TRUE)
  
  return(FALSE)
}


next_state <- function(state, action){
  new_state <- state + action
  return(new_state)
}


full_back <- function(state, action, discount, V){
  x <- (state-1)%%4 + 1
  y <- (state-1)%/%4 + 1
  
  state <- c("x" = x, "y" = y)
  
  if(is_off_grid(state, action)) {
    r <- -1
    new_state <- state
  } else {
    r <- -1
    new_state <- next_state(state, action)
  }
  val <- r + discount * V[ new_state[["x"]], new_state[["y"]] ]
  return(val)
}


compute_V <- function(discount = 1){
  V <- new_V <- matrix(0, nrow = rows, ncol = cols)
  V_k <- matrix(0, nrow = 1000, ncol = (rows*cols))
  
  for(k in 1:1000){
    for(state in 2:((rows*cols) - 1)){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      new_V[state] <- mean(vals)
      V_k[k, ] <- V[state]
    }
    temp <- V
    V <- new_V
    new_V <- temp
  }
  return(new_V)
}



compute_V_star <- function(discount = 1, return_best_actions = F){
  V <- new_V <- matrix(0, nrow = rows, ncol = cols)
  V_k <- matrix(0, nrow = 1000, ncol = (rows*cols))
  
  best_actions <- matrix("T", nrow = rows, ncol = cols)
  action_names <- names(actions)
  
  for(k in 1:1000){
    for(state in 2:((rows*cols) - 1)){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      new_V[state] <- max(vals)
      V_k[k, ] <- V[state]
      
      act <- which(vals == max(vals))
      best_actions[state] <- paste(action_names[act], collapse = "/")
    }
    temp <- V
    V <- new_V
    new_V <- temp
  }
  
  if(return_best_actions)
    return(list("values" = V, "best_actions" = best_actions))
  
  return(new_V)
}



compute_V()

compute_V_star(return_best_actions = T)







