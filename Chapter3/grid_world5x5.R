library(tidyverse)
library(reshape2)

actions <- list(
  "U" = c(-1, 0),
  "D" = c(1, 0),
  "L" = c(0, -1),
  "R" = c(0, 1)
)

A <- c("x" = 1, "y" = 2)
B <- c("x" = 1, "y" = 4)

A_prime <- c("x" = 5, "y" = 2)
B_prime <- c("x" = 3, "y" = 4)


cols <- 5
rows <- 5

ifelse(state%%5 == 0, state%%5 , state%/%5)

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
  x <- (state-1)%%5 + 1
  y <- (state-1)%/%5 + 1
 
  state <- c("x" = x, "y" = y)
  
  if(all(state == A)) {
    r <- 10
    new_state <- A_prime
  } else if(all(state == B)) {
    r <- 5
    new_state <- B_prime
  } else if(is_off_grid(state, action)) {
    r <- -1
    new_state <- state
  } else {
    r <- 0
    new_state <- next_state(state, action)
  }
  val <- r + discount * V[ new_state[["x"]], new_state[["y"]] ]
  return(val)
}



compute_V <- function(discount = 0.9){
  V <- matrix(0, nrow = rows, ncol = cols)
  
  while(TRUE){
    old_v <- V
    for(state in seq_len(rows * cols)){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      V[state] <- mean(vals)
    }
    
    delta <- sum(abs(old_v - V))
    
    if(delta < 0.000001)
      break
  }
  return(V)
}


compute_V_star <- function(discount = 0.9, return_best_actions = F){
  V <- matrix(0, nrow = rows, ncol = cols)
  best_actions <- matrix("", nrow = rows, ncol = cols)
  action_names <- names(actions)
  while(TRUE){
    old_v <- V
    for(state in seq_len(rows * cols)){
      vals <- map_dbl(actions, full_back, state = state, discount = discount, V = V)
      V[state] <- max(vals)
      act <- which(vals == max(vals))
      best_actions[state] <- paste(action_names[act], collapse = "/")
    }
    
    delta <- sum(abs(old_v - V))
    
    if(delta < 0.000001)
      break
  }
  if(return_best_actions)
    return(list("values" = V, "best_actions" = best_actions))
  return(V)
}


result_V <- compute_V()
round(result_V, 2)

result_V_star <- compute_V_star(return_best_actions = T)
round(result_V_star$values, 2)
result_V_star$best_actions


# ---- plotting figures


plot_fig3.2 <- function(discount = 0.9){
  
  result <- compute_V(discount = discount)
  result_df <- melt(round(result, 2))
  
  plot <- ggplot(result_df, aes(Var2, Var1)) + geom_tile(aes(fill = value), colour = "white") + 
    geom_text(aes(label = value)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "",y = "") +
    scale_y_reverse()
  
  return(plot)
}


plot_fig3.5 <- function(discount = 0.9){
  
  result <- compute_V_star(discount = discount)
  result_df <- melt(round(result, 2))
  
  plot <- ggplot(result_df, aes(Var2, Var1)) + geom_tile(aes(fill = value), colour = "white") + 
    geom_text(aes(label = value)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "",y = "") +
    scale_y_reverse()
  
  return(plot)
}


plot_fig3.2()
plot_fig3.5()
