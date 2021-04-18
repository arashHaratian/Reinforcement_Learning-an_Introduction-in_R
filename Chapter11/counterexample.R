library(tidyverse)
library(patchwork)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]



# Moore-Penrose pseudo-inverse of a matrix
MPI <- function(matrix){
  SVD <- svd(matrix)
  positive <- (SVD$d > max( 1e-16 * SVD$d[1], 0))
  if (all(positive)) {
    inverse <- SVD$v %*% (1/SVD$d * t(SVD$u))
  }
  else if (any(positive)) {
    inverse <- SVD$v[, positive, drop = FALSE] %*% (1/SVD$d[positive] * t(SVD$u[, positive, drop = FALSE]))
  }
  else {
    inverse <- matrix(0, nrow = ncol(A), ncol = nrow(A))
  }
  return(inverse)
}



num_of_states <- 7



initialize_features <- function(with_weight = FALSE){
  features <- matrix(0, nrow = num_of_states, ncol = num_of_states + 1)
  for(i in seq_len(num_of_states)){
    features[i, i] <- 2
    features[i, num_of_states + 1] <- 1
  }
  features[num_of_states, num_of_states] <- 1
  features[num_of_states, num_of_states + 1] <- 2
  features <<- features
  
  theta <- rep(1, num_of_states + 1)
  theta[[num_of_states]] <- 10
  theta <<- theta
  
  if(with_weight){
    weight <<- vector("double", num_of_states + 1)
  }
}



off_policy_semi_gradient_TD <- function(state, alpha, discount = 0.99){
  
  
  if(runif(1) < 1/num_of_states){
    # the solid action
    new_state <- num_of_states
    rho <- 1 / (1/num_of_states)
    
  } else{
    # the dashed action
    new_state <- resample(1:(num_of_states - 1), 1)
    rho <- 0
  }
  
  
  delta <- discount * (features[new_state, ] %*% theta) - (features[state, ] %*% theta)
  delta <- delta * rho * alpha   
  
  theta <<- theta + as.vector(features[state, ]) * as.vector(delta)
  
  return(new_state)
}



semi_gradient_DP <- function(alpha, discount = 0.99){
  
  delta <- 0
  
  for(state in seq_len(num_of_states)){
    expected_return <- 0
    
    for(new_state in seq_len(num_of_states))
      if(new_state == num_of_states)
        expected_return <- expected_return  + discount * theta %*% features[new_state, ]
    
    error <- expected_return - theta %*% features[state, ]
    delta <- delta + as.vector(error) * as.vector(features[state, ])
  }
  theta <<- theta + (alpha / num_of_states * delta)
}



TDC <- function(state, alpha, beta, discount = 0.99){
  if(runif(1) < 1/num_of_states){
    # the solid action
    new_state <- num_of_states
    rho <- 1 / (1/num_of_states)
    
  } else{
    # the dashed action
    new_state <- resample(1:(num_of_states - 1), 1)
    rho <- 0
  }
  
  delta <- discount * (features[new_state, ] %*% theta)  - (features[state, ] %*% theta)
  delta <- as.vector(delta)
  theta <<- theta + alpha * rho *
    (delta * features[state, ] - discount * features[new_state, ] * 
       as.vector(features[state, ] %*% weight))
  
  weight <<- weight + beta * rho * (delta - as.vector(features[state, ] %*% weight)) * as.vector(features[state, ])
  return(new_state)
}



expected_TDC <- function(alpha, beta, discount = 0.99){
  for(state in seq_len(num_of_states)){
    delta <- discount * (features[num_of_states, ] %*% theta) - (features[state, ] %*% theta)
    delta <- as.vector(delta)
    rho <- 1 / (1/num_of_states)
    expected_update_theta <- 1 / num_of_states * (1/num_of_states) * rho * 
      (delta * features[state, ] - discount * features[num_of_states, ] * as.vector(weight %*% features[state, ]))
    
    theta <<- theta + alpha * expected_update_theta
    
    expected_update_weight <- 1 / num_of_states * (1/num_of_states) * rho * 
      (delta - as.vector(weight %*% features[state, ])) * features[state, ]
    
    weight <<- weight + beta * expected_update_weight
  }
}



expected_emphatic_TD <- function(emphasis, alpha, discount = 0.99, interest = 1){
  expected_update <- 0
  expected_new_emphasis <- 0
  
  for(state in seq_len(num_of_states)){
    if(state == num_of_states){
      rho <- 1 / (1/num_of_states)
    }else{
      rho <- 0
    }
    new_emphasis <- discount * rho * emphasis + interest
    expected_new_emphasis <- expected_new_emphasis + new_emphasis
    new_state <- num_of_states
    delta <- discount * as.vector(features[new_state, ] %*% theta) - as.vector(features[state, ] %*% theta)
    expected_update <- expected_update + 
      (1 / num_of_states * (1/num_of_states) * new_emphasis * 1 / (1/num_of_states) * delta * features[state, ])
  }
  theta <<- theta + alpha * expected_update
  

  return (expected_new_emphasis / num_of_states)
}



RMSVE <- function(){
  state_distribution <- rep(1/num_of_states, num_of_states)
  return(sqrt(as.vector((features %*% theta) ^ 2) %*% state_distribution))
}



RMSPBE <- function(discount = 0.99){
  error <- vector("double", num_of_states)
  state_distribution <- rep(1/num_of_states, num_of_states)
  
  for(state in seq_len(num_of_states)){
    for (new_state in seq_len(num_of_states))
      if (new_state == num_of_states)
        error[state] <- error[state] + (discount * (theta %*% features[new_state, ]) - (theta %*% features[state, ]))
  }
  
  projection <- features %*% MPI(t(features) %*% diag(state_distribution) %*% features) %*%
    t(features) %*% diag(state_distribution) 
  
  
  error <- projection %*% error
  
  
  return(sqrt(as.vector(error ^ 2) %*% state_distribution))
}



# plotting figures

plot_fig11.2 <- function(){
  # left plot
  initialize_features()
  
  state <- resample(seq_len(num_of_states), 1)

  theta_list <- list()
  for(step in 1:1000){
    state <- off_policy_semi_gradient_TD(state, 0.01)
    theta_list[[step]] <- theta
  }

  lines <- theta_list %>% 
    transpose() %>% 
    imap(~geom_line(aes(y = unlist(.x), color = as.character(.y))))
  
  x_axis <- data.frame("steps" = 1:1000)
  
  plot1 <- ggplot(x_axis, aes(x = steps)) +
    lines +
    labs(title = "semi-gradient off-policy TD",
         color = "theta",
         y = "value")
  
  
  #right plot
  initialize_features()
  

  theta_list <- list()
  for(sweep in 1:1000){
    semi_gradient_DP(0.01)
    theta_list[[sweep]] <- theta
  }
  
  lines <- theta_list %>% 
    transpose() %>%
    imap(~{
      color <- ifelse(.y %in% 1:6, "1-6", as.character(.y))
      geom_line(aes(y = unlist(.x), color = color))
    })
  
  x_axis <- data.frame("sweeps" = 1:1000)
  
  plot2 <- ggplot(x_axis, aes(x = sweeps)) +
    lines +
    labs(title = "semi-gradient DP",
         color = "theta",
         y = "value")
  
  return(wrap_plots(plot1, plot2))
}



plot_fig11.5 <- function(){
  # left plot
  initialize_features(with_weight = TRUE)
  
  state <- resample(seq_len(num_of_states), 1)
  
  theta_list <- list()
  rmsve <- vector("double", 1000)
  rmspbe  <- vector("double", 1000)
  for(step in 1:1000){
    state <- TDC(state, 0.005, 0.05)
    theta_list[[step]] <- theta
    rmsve[step] <- RMSVE()
    rmspbe[step] <- RMSPBE()
  }
  
  
  rmsve <- data.frame(rmsve, "steps" = 1:1000)
  rmspbe <- data.frame(rmspbe, "steps" = 1:1000)
  
  lines <- theta_list %>% 
    transpose() %>% 
    imap(~geom_line(aes(y = unlist(.x), color = paste("theta", .y))))
  
  x_axis <- data.frame("steps" = 1:1000)
  
  plot1 <- ggplot(x_axis, aes(x = steps)) +
    lines +
    geom_line(aes(y = rmspbe, color = "RMSPBE"), rmspbe) +
    geom_line(aes(y = rmsve, color = "RMSVE"), rmsve) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "TDC",
         y = "value",
         color = "")
  
  
  # right plot
  initialize_features(with_weight = TRUE)
  
  
  theta_list <- list()
  rmsve <- vector("double", 1000)
  rmspbe  <- vector("double", 1000)
  for(sweep in 1:1000){
    state <- expected_TDC(0.005, 0.05)
    theta_list[[sweep]] <- theta
    rmsve[sweep] <- RMSVE()
    rmspbe[sweep] <- RMSPBE()
  }
  
  rmsve <- data.frame(rmsve, "sweeps" = 1:1000)
  rmspbe <- data.frame(rmspbe, "sweeps" = 1:1000)
  
  lines <- theta_list %>% 
    transpose() %>%
    imap(~{
      color <- ifelse(.y %in% 1:6, "theta 1-6", paste("theta", .y))
      geom_line(aes(y = unlist(.x), color = color))
    })
  
  x_axis <- data.frame("sweeps" = 1:1000)
  
  plot2 <- ggplot(x_axis, aes(x = sweeps)) +
    lines +
    geom_line(aes(y = rmspbe, color = "RMSPBE"), rmspbe) +
    geom_line(aes(y = rmsve, color = "RMSVE"), rmsve) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "expected TDC",
         y = "value",
         color = "")
  
  return(wrap_plots(plot1, plot2))
}



plot_fig11.6 <- function(){
  
  initialize_features()
  
  emphasis <- 0
  theta_list <- list()
  rmsve <- vector("double", 1000)
  
  for(sweep in 1:1000){
    emphasis <- expected_emphatic_TD(emphasis, 0.03)
    theta_list[[sweep]] <- theta
    rmsve[sweep] <- RMSVE()
  }
  rmsve <- data.frame(rmsve, "sweeps" = 1:1000)
  
  lines <- theta_list %>% 
    transpose() %>%
    imap(~{
      color <- ifelse(.y %in% 1:6, "theta 1-6", paste("theta", .y))
      geom_line(aes(y = unlist(.x), color = color))
    })
  
  x_axis <- data.frame("sweeps" = 1:1000)
  
  plot <- ggplot(x_axis, aes(x = sweeps)) +
    lines +
    geom_line(aes(y = rmsve, color = "RMSVE"), rmsve) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "emphatic-TD",
         y = "value",
         color = "")
  
  return(plot)
}



plot_fig11.2()
plot_fig11.5()
plot_fig11.6()
