library(tidyverse)
library(patchwork)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


initialize_params <- function(n, b){
  R <- array(rnorm(n * 2 * b), dim = c(n, 2, b))                    
  Q <- matrix(0, nrow = n, ncol = 2)
  successor <- array(dim = c(n, 2, b))
  for(s in seq_len(n)){
    for(a in 1:2)
    successor[s, a, ] <- resample(seq_len(n), b)
  }
  
  return(list("R" = R, "Q" = Q, "successor" = successor))
}


next_state <- function(state, action, successor){
  if(runif(1, 0, 1) < 0.9){
    next_state <- resample(successor[state, action, ], 1)
    return(next_state)
  } else {
    return(dim(successor)[1])
  }
}


policy_eval <- function(R, Q, successor, runs = 1000){
  results <- vector("double")
  n <- nrow(Q)
  
  for(run in seq_len(runs)){
    state <- 1
    rewards <- 0
    while(state < n){
      action <- resample(which(Q[state, ] == max(Q[state, ])), 1)  # max Q with tie break
      new_state <- next_state(state, action, successor)
      reward <- R[state, action, which(successor[state, action, ] == new_state)]
      if(new_state == n)
        reward <- 0
      rewards <- rewards + reward
      state <- new_state
    }
    results[run] <- rewards
  }
  results <- mean(results)
  return(results)
}


uniform <- function(n, b, eval_each = 100, num_steps = 20000){
  performance <- vector("double")
  params <- initialize_params(n, b)
  R <- params[["R"]]
  Q <- params[["Q"]]
  successor <- params[["successor"]]
  
  for(step in seq(0, num_steps)){
    state <- (step %/% 2 %% n) + 1
    action <- (step %% 2) + 1
    
    next_states <- successor[state, action, ]
    Q[state, action] <- 0.9 * mean(R[state, action, ] + max(Q[next_states, ]))
    
    if(step %% eval_each == 0){
      v <- policy_eval(R, Q, successor)
      performance <- append(performance, v)
      }
  }
  return(performance)
}


on_policy <- function(n, b, epsilon, eval_each = 100, num_steps = 20000){
  performance <- vector("double")
  params <- initialize_params(n, b)
  R <- params[["R"]]
  Q <- params[["Q"]]
  successor <- params[["successor"]]
  state <- 1
  for(step in seq(0, num_steps)){
    if(runif(1, 0, 1) < epsilon){
      action <- resample(1:2, 1)
    }else {
      action <-  resample(which(Q[state, ] == max(Q[state, ])), 1)
    }
    new_state <- next_state(state, action, successor)
    next_states <- successor[state, action, ]
    Q[state, action] <- 0.9 * mean(R[state, action,] + max(Q[next_states, ]))
    
    if(new_state == n)
      new_state <- 1
    
    state <- new_state
    
    
    if(step %% eval_each == 0){
      v <- policy_eval(R, Q, successor)
      performance <- append(performance, v)
    }
  }
  return(performance)
}




# plotting figure 8.8

plot_fig8.8 <- function(){


result_uniform <- list()
result_on_policy <- list()

bs <- c(1, 3, 10)

for(b in bs){
  performance_uniform <- 0
  performance_on_policy <- 0
  for(i in 1:30){
    print(i)
    performance_uniform <- performance_uniform + uniform(1000, b)
    performance_on_policy <- performance_on_policy + on_policy(1000, b, 0.1)
  }
  result_uniform[[as.character(b)]] <- performance_uniform/30
  result_on_policy[[as.character(b)]] <- performance_on_policy/30
}



lines_uniform <- result_uniform %>% 
  imap(~geom_line(aes(y = .x, color = .y)))
lines_on_policy <- result_on_policy %>% 
  imap(~geom_line(aes(y = .x, color = .y), linetype = "longdash"))


x_axis <- data.frame("steps" = 1:201)

P1 <- ggplot(x_axis, aes(x = steps)) +
  lines_uniform +
  lines_on_policy +
  scale_x_continuous(labels = c("0", "50000", "100000", "150000", "200000"), breaks = c(0, 50, 100, 150, 200)) +
  labs(x = "computation time, in expected updates",
       y = "value of start state",
       color = "b", 
       title = "1000 states") 
  
  


result_uniform <- list()
result_on_policy <- list()

performance_uniform <- 0
performance_on_policy <- 0
for(i in 1:30){
  print(i)
  performance_uniform <- performance_uniform + uniform(10000, 1)
  performance_on_policy <- performance_on_policy + on_policy(10000, 1, 0.1)
}
result_uniform[["1"]] <- performance_uniform/30
result_on_policy[["1"]] <- performance_on_policy/30

lines_uniform <- result_uniform %>% 
  imap(~geom_line(aes(y = .x, color = .y)))
lines_on_policy <- result_on_policy %>% 
  imap(~geom_line(aes(y = .x, color = .y), linetype = "longdash"))


P2 <- ggplot(x_axis, aes(x = steps)) +
  lines_uniform +
  lines_on_policy +
  scale_x_continuous(labels = c("0", "50000", "100000", "150000", "200000"), breaks = c(0, 50, 100, 150, 200)) +
  labs(x = "computation time, in expected updates",
       y = "value of start state",
       color = "b", 
       title = "10000 states")

return(wrap_plots(P1, P2))
}



# plot_fig8.8()
