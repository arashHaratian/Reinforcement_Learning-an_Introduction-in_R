library(tidyverse)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]

# making an k-armed bandits environment
bandit <- function(k = 10, mean = 0, sd = 1, reward_baseline = 0){
  # this function will keep the true rewards 
  #and a function will be returned for interacting with the bandits and receiving the reward.
  true_means <- rnorm(k, mean + reward_baseline, sd)
  function(action, return_true_mean = F){
    reward <- rnorm(1, mean = true_means[action], sd = 1)
    if(return_true_mean)
      return(list("reward" = reward, "true_means" = true_means))
    return(reward)
  }
} # ten_bandits_instance <- bandit(k = 10)



# an agent for interacting with the bandits
bandit_agent <- function(k = 10,  # number of arms
                         eps = 0, # epsilon (prob of acting on random)
                         steps = 1000, # number of times to interacting with one env
                         initial_action_value = 0, # value of the Q
                         learning_function = learn_sample_average,  # update rule function
                         step_size = 0.1,  # constant step size in 'learn_step_size()' update rule (instead of sample average)
                         UCB_degree = NULL, # 'C' in UCB (degree of exploration)
                         add_to_reward = 0, # add a baseline to the true rewards
                         is_gradient = F,   # using gradient bandit algorithm
                         is_gradient_baseline = F # using average reward as a baseline in gradient
                         ) {
  # initializing the Q and the N
  Q <<- rep(initial_action_value, k)
  N <<- vector("integer", k)
  
  rewards <- vector("double", steps) # for storing the rewards of the actions
  actions <- vector("double", steps) # for storing the count of the best action
  
  Qtemp <- rep(initial_action_value, k) # for storing the result of the UCB
  
  prob <- runif(steps) # prob of taking random action in each step (if prob < epsilon then explore)
  
  bandits_instance <- bandit(k = 10, reward_baseline = add_to_reward) # making an K-armed bandits environment
  best_action <- which.max(bandits_instance(1, TRUE)$true_means)  # storing the best action of the environment
  
  for (step in seq_len(steps)) {
    if(!is.null(UCB_degree)){ # if 'UCB_degree' specified then it will use the UCB
      time <- sum(N, 1) # == sum(N) + 1
      Qtemp <- Q + (UCB_degree * sqrt(log(time) / (N + 1e-5)))
      
      action <- ifelse(prob[[step]] > eps, resample(which(Qtemp == max(Qtemp)), 1), resample(1:k, 1))
      reward <- bandits_instance(action)
      learning_function(action, reward, step_size)
      rewards[step] <- reward
      if(action == best_action)
        actions[step] <- 1
    } else if(is_gradient){ # if 'is_gradient == TRUE' then it will use the gradient based bandit algorithm
      Q_exp <- exp(Q)
      policy <- Q_exp / sum(Q_exp)
      action <- resample(seq_along(Q), 1, prob = policy)
      reward <- bandits_instance(action)
      rewards[step] <- reward
      if(action == best_action)
        actions[step] <- 1
      one_hot <- rep(0, length(Q))
      one_hot[action] <- 1
      if (is_gradient_baseline){
        rewards_until_now <- rewards[rewards != 0]
        baseline <- mean(rewards_until_now)
        Q <<- Q + step_size * (reward - baseline) * (one_hot - policy)
      } else {
        Q <<- Q + step_size * (reward) * (one_hot - policy)
      }
      
    } else{ #if non of the above then the specified learning rule ('learn_sample_average()' or 'learn_step_size()') will be used
      action <- ifelse(prob[[step]] > eps, resample(which(Q == max(Q)), 1), resample(1:k, 1))
      reward <- bandits_instance(action)
      learning_function(action, reward, step_size)
      rewards[step] <- reward
      if(action == best_action)
        actions[step] <- 1
    }
  }
  return(list("rewards" = rewards, "actions" = actions))
}


# update the Q with sample average
learn_sample_average <- function(action, reward, ...) {
  # it should be passed as an argument to the agent function
  N[action] <<- N[action] + 1
  Q[action] <<- Q[action] + (1 / N[action] * (reward - Q[action]))
}

# update the Q with constant step size 
learn_step_size <- function(action, reward, step_size, ...) {
  # it should be passed as an argument to the agent
  N[action] <<- N[action] + 1
  Q[action] <<- Q[action] + (step_size * (reward - Q[action]))
}







## figure 2.1

plot_fig2.1 <- function(n = 10, runs = 2000){
  
  bandits_instance <- bandit(k = n)
  
  mat <- matrix(nrow = runs, ncol = n)
  for(action in seq_len(n))
    for(i in seq_len(runs)){
      mat[i, action] <- bandits_instance(action)
    }
  
  true_means <- bandits_instance(1, T)$true_means
  true_means_df <- data.frame(actions = seq_len(n), value = true_means)
  df <- mat %>% 
    as.data.frame() %>% 
    rename_with(~paste0("action", seq_len(n)), .cols = everything()) %>% 
    pivot_longer(cols = everything(), names_to = "actions") %>% 
    mutate(actions = as_factor(actions))
  
  plot <- ggplot(df) +
    geom_violin(aes(x = actions, y = value, fill = actions)) +
    geom_point(aes(x = actions, y = value), data = true_means_df)+
    theme(axis.text.x = element_text(angle = 90)) + 
    ylab("reward distribution")
  
  
  return(plot)
}


## figure 2.2
# reward

plot_fig2.2.1 <- function(runs = 2000, steps = 1000) {
  
  mat1 <- mat2 <- mat3 <- matrix(nrow = runs , ncol = steps)
  for(i in seq_len(runs)){
    mat1[i, ] <- bandit_agent(eps = 0.01, steps = steps)$rewards
    mat2[i, ] <- bandit_agent(eps = 0.1, steps = steps)$rewards
    mat3[i, ] <- bandit_agent(eps = 0, steps = steps)$rewards
  }
  
  df <- tibble(
    "eps0.01" = colMeans(mat1),
    "eps0.1" = colMeans(mat2),
    "eps0" = colMeans(mat3),
    "steps" = seq_len(steps)
  )
  
  plot <- ggplot(df, aes(x = steps)) +
    geom_line(aes(y = eps0.01, color = "eps = 0.01")) +
    geom_line(aes(y = eps0.1, color = "eps = 0.1")) +
    geom_line(aes(y = eps0, color = "eps = 0")) +
    ylab("average reward") +
    scale_color_manual(breaks = c("eps = 0.1", "eps = 0.01", "eps = 0"), values = c("blue", "red", "darkgreen"))
  
  return(plot)
}


#action

plot_fig2.2.2 <- function(runs = 2000, steps = 1000) {
  
  mat1 <- mat2 <- mat3 <- matrix(nrow = runs , ncol = steps)
  for(i in seq_len(runs)){
    mat1[i, ] <- bandit_agent(eps = 0.01, steps = steps)$actions
    mat2[i, ] <- bandit_agent(eps = 0.1, steps = steps)$actions
    mat3[i, ] <- bandit_agent(eps = 0, steps = steps)$actions
  }
  
  df <- tibble(
    "eps0.01" = colMeans(mat1),
    "eps0.1" = colMeans(mat2),
    "eps0" = colMeans(mat3),
    "steps" = seq_len(steps)
  )
  
  plot <- ggplot(df, aes(x = steps)) +
    geom_line(aes(y = eps0.01, color = "eps = 0.01")) +
    geom_line(aes(y = eps0.1, color = "eps = 0.1")) +
    geom_line(aes(y = eps0, color = "eps = 0")) +
    ylab("% optimal action")+
    scale_color_manual(breaks = c("eps = 0.1", "eps = 0.01", "eps = 0"), values = c("blue", "red", "darkgreen"))
  
  return(plot)
}



##figure 2.3

plot_fig2.3 <- function(runs = 2000, steps = 1000) {
  
  
  mat1 <- mat2 <- matrix(nrow = runs , ncol = steps)

  for (i in seq_len(runs)){
    mat1[i, ] <- bandit_agent(eps = 0, steps = steps, initial_action_value = 5, learning_function = learn_step_size)$actions
    mat2[i, ] <- bandit_agent(eps = 0.1, steps = steps, learning_function = learn_step_size)$actions
  }

  df <- tibble(
    "init5" = colMeans(mat1),
    "eps0.1" = colMeans(mat2),
    "steps" = seq_len(steps)
  )
  
  plot <- ggplot(df, aes(x = steps)) +
    geom_line(aes(y = init5, color = "init = 5, eps = 0")) +
    geom_line(aes(y = eps0.1, color = "init = 0, eps = 0.1")) +
    ylab("% optimal action") +
    theme(legend.position="bottom")
  
  return(plot)
}


##figure 2.4

plot_fig2.4 <- function(runs = 2000, steps = 1000) {
  
  
  mat1 <- mat2 <- matrix(nrow = runs , ncol = steps)
  for(i in seq_len(runs)){
    mat1[i, ] <- bandit_agent(eps = 0, steps = steps, UCB_degree = 2)$rewards
    mat2[i, ] <- bandit_agent( eps = 0.1, steps = steps)$rewards
  }

  df <- tibble(
    "UCB" = colMeans(mat1),
    "eps0.1" = colMeans(mat2),
    "steps" = seq_len(steps)
  )
  
  plot <- ggplot(df, aes(x = steps)) +
    geom_line(aes(y = eps0.1, color = "eps = 0.1")) +
    geom_line(aes(y = UCB, color =  "UCB")) +
    scale_color_manual(breaks = c("UCB", "eps = 0.1"), values = c("darkblue", "lightgreen")) +
    ylab("average reward")
  
  return(plot)
}


##figure 2.5

plot_fig2.5 <- function(runs = 2000, steps = 1000) {
  
  mat1 <- mat2 <- mat3 <- mat4 <- matrix(nrow = runs , ncol = steps)

  for(i in seq_len(runs)){
    mat1[i, ] <- bandit_agent(step_size = 0.1, is_gradient = T, is_gradient_baseline = T, add_to_reward = 4)$actions
    mat2[i, ] <- bandit_agent(step_size = 0.1, is_gradient = T, is_gradient_baseline = F, add_to_reward = 4)$actions
    mat3[i, ] <- bandit_agent(step_size = 0.4, is_gradient = T, is_gradient_baseline = T, add_to_reward = 4)$actions
    mat4[i, ] <- bandit_agent(step_size = 0.4, is_gradient = T, is_gradient_baseline = F, add_to_reward = 4)$actions
  }

  df <- tibble(
    "a0.1b" = colMeans(mat1),
    "a0.1" = colMeans(mat2),
    "a0.4b" = colMeans(mat3),
    "a0.4" = colMeans(mat4),
    "steps" = seq_len(steps)
  )
  
  plot <- ggplot(df, aes(x = steps)) +
    geom_line(aes(y = a0.1b, color = "alpha = 0.1, with_baseline")) +
    geom_line(aes(y = a0.1, color = "alpha = 0.1")) +
    geom_line(aes(y = a0.4b, color = "alpha = 0.4, with_baseline")) +
    geom_line(aes(y = a0.4, color = "alpha = 0.4")) +
    scale_color_manual(
      breaks = c(
        "alpha = 0.1, with_baseline",
        "alpha = 0.4, with_baseline",
        "alpha = 0.1",
        "alpha = 0.4"
      ),
      values = c("darkblue", "lightblue", "darkgreen", "green")) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
    labs(y = "% optimal action", subtitle = "the true rewards were sampled from a normal distribution\nwith a mean of +4 instead of zero")
  
  return(plot)
}



##figure 2.6

plot_fig2.6 <- function(runs = 2000, steps = 1000) {
  epsilons <- c(2 ^ (-7:-2))
  step_sizes <- c(2 ^ (-5:1))
  UCB_degrees <- c(2 ^ (-4:2))
  init_values <- c(2 ^ (-2:2))
  
  
  mat1 <- matrix(nrow = runs , ncol = length(epsilons))
  mat2 <- matrix(nrow = runs , ncol = length(step_sizes))
  mat3 <- matrix(nrow = runs , ncol = length(UCB_degrees))
  mat4 <- matrix(nrow = runs , ncol = length(init_values))
  
  for(i in seq_len(runs)){
    mat1[i, ] <- colMeans(as.data.frame(map(epsilons, ~bandit_agent(eps = .x, steps = steps)$rewards)))
    df1 <- tibble("eps" = colMeans(mat1), "param" = -7:-2)
    
    mat2[i, ] <- colMeans(as.data.frame(map(step_sizes, ~bandit_agent(step_size = .x, is_gradient = T, is_gradient_baseline = T, steps = steps)$rewards)))
    df2 <- tibble("step_size" = colMeans(mat2), "param" = -5:1)
    
    mat3[i, ] <- colMeans(as.data.frame(map(UCB_degrees, ~bandit_agent(UCB_degree = .x, eps = 0, steps = steps)$rewards)))
    df3 <- tibble("UCB" = colMeans(mat3), "param" = -4:2)
    
    mat4[i, ] <- colMeans(as.data.frame(map(init_values, ~bandit_agent(initial_action_value = .x, learning_function = learn_step_size, steps = steps)$rewards)))
    df4 <- tibble("init" = colMeans(mat4), "param" = -2:2)
  }
  
  plot <- ggplot() +
    geom_line(aes(x = param, y = eps, color = "epsilon greedy"), data = df1) +
    geom_line(aes(x = param, y = step_size, color = "gradient bandit"), data = df2) +
    geom_line(aes(x = param, y = UCB, color = "UCB"), data = df3) +
    geom_line(aes(x = param, y = init, color = "optimistic initialization"), data = df4) +
    scale_color_manual(
      breaks = c(
        "epsilon greedy",
        "gradient bandit",
        "UCB",
        "optimistic initialization"
      ),
      values = c("red", "green", "darkblue", "black")) +
    labs(x = bquote(~2^{parameters}), y = "average reward\nover first 1000 steps")
  
  
  return(plot)
}





# plot_fig2.1()
# plot_fig2.2.1()
# plot_fig2.2.2()
# plot_fig2.3()
# plot_fig2.4()
# plot_fig2.5()
# plot_fig2.6()