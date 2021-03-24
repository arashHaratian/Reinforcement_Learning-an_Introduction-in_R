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


compute_V <- function(discount = 1, return_all_iters = F){
  V <- new_V <- matrix(0, nrow = rows, ncol = cols)
  V_k <- matrix(0, nrow = 1000, ncol = (rows*cols))
  non_terminal_states <- (rows*cols) - 1
  
  k <- 0
  
  while(TRUE) {
    for(state in 2:non_terminal_states){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      new_V[state] <- mean(vals)
    }
    #saving the Values of this iteration
    V_k[k, ] <- V
    
    #swap the values
    temp <- V
    V <- new_V
    new_V <- temp
    
    delta <- sum(abs(new_V - V))
    if(delta < 0.000001)
      break
    
    k <- k + 1
  }
  V_k <- V_k[1:k, ] #cutting empty rows
  
  if(return_all_iters)
    return(list("last_values" = V, "all_iterations" = V_k))
  
  return(V)
}




compute_V_star <- function(discount = 1, return_best_actions = F){
  V <- new_V <- matrix(0, nrow = rows, ncol = cols)
  
  best_actions <- matrix("T", nrow = rows, ncol = cols)
  action_names <- names(actions)
  non_terminal_states <- (rows*cols) - 1
  
  while(TRUE) {
    for(state in 2:non_terminal_states){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      new_V[state] <- max(vals)
      
      #saving the best action in each state
      act <- which(vals == max(vals))
      best_actions[state] <- paste(action_names[act], collapse = "/")
    }
    #swap the values
    temp <- V
    V <- new_V
    new_V <- temp
    
    delta <- sum(abs(new_V - V))
    if(delta < 0.000001)
      break
  }
  
  if(return_best_actions)
    return(list("values" = V, "best_actions" = best_actions))
  
  return(V)
}




# plotting figure 4.1
plot_fig4.1 <- function(discount = 1){
  
  result <- compute_V(discount = discount)
  result_df <- melt(round(result, 2))
  
  plot <- ggplot(result_df, aes(Var2, Var1)) + geom_tile(aes(fill = value), colour = "white") + 
    geom_text(aes(label = value)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "",y = "") +
    scale_y_reverse()
  
  return(plot)
}




# let's try the code

#values
compute_V()
compute_V_star()

#first 3 iteration values
iters_vals <- compute_V(return_all_iters = T)$all_iterations
map(.x = 1:3, ~matrix(iters_vals[.x, ], nrow = 4))

#best_policy
compute_V_star(return_best_actions = T)

#plot
plot_fig4.1()

