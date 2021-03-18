library(purrr)
library(ggplot2)

resample <- function(x, ...)
  x[sample.int(length(x), ...)]

# making an k-armed bandits
bandit <- function(k = 10, mean = 0, sd = 1, reward_baseline){
  # this function will keep the true rewards 
  #and a function will be returned for interacting with the bandits and receiving the reward.
  true_means <- rnorm(k, mean + reward_baseline, sd)
  print(true_means)
  function(action, return_true_mean = F){
    reward <- rnorm(1, mean = true_means[action], sd = 1)
    if(return_true_mean)
      return(list("reward" = reward, "true_means" = true_means))
    return(reward)
  }
} # ten_bandits_instance <- bandit(k = 10)




bandit_agent <- function(k = 10,
                         eps = 0,
                         steps = 1000,
                         initial_action_value = 0,
                         learning_function = learn_sample_average,
                         step_size = 0.1,
                         UCB_degree = NULL,
                         add_to_reward = 0,
                         is_gradient = F,
                         is_gradient_baseline = F) {
  
  Q <<- rep(initial_action_value, k)
  N <<- vector("integer", k)
  rewards <- vector("double", steps)
  actions <- vector("double", steps)
  
  Qtemp <- rep(initial_action_value, k)
  
  prob <- runif(steps)
  
  bandits_instance <- bandit(k = 10, reward_baseline = add_to_reward)
  best_action <- which.max(bandits_instance(1, TRUE)$true_means)
  
  for (step in seq_len(steps)) {
    if(!is.null(UCB_degree)){
      time <- sum(N, 1) # == sum(N) + 1
      Qtemp <- Q + (UCB_degree * sqrt(log(time) / (N + 1e-5)))
      
      action <- ifelse(prob[[step]] > eps, resample(which(Qtemp == max(Qtemp)), 1), resample(1:k, 1))
      reward <- bandits_instance(action)
      learning_function(action, reward, step_size)
      rewards[step] <- reward
      if(action == best_action)
        actions[step] <- 1
    } else if(is_gradient){
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
      
    } else{
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


learn_sample_average <- function(action, reward, ...) {
  N[action] <<- N[action] + 1
  Q[action] <<- Q[action] + (1 / N[action] * (reward - Q[action]))
}

learn_step_size <- function(action, reward, step_size, ...) {
  N[action] <<- N[action] + 1
  Q[action] <<- Q[action] + (step_size * (reward - Q[action]))
}

# learn_gradient <- function(with_baseline = F, step_size, r, baseline){
#   one_hot <- rep(0, length(Q))
#   one_hot[action] <- 1
#   if (with_baseline){
#     Q <<- Q + step_size * (r - baseline) * (one_hot - policy)
#   } else {
#     Q <<- Q + step_size * (reward) * (one_hot - policy)
#   }
# }





## figure 1
n <- 10
ten_bandits_instance <- bandit(k = n)


mat <- matrix(nrow = 1000, ncol = n)
for(action in seq_len(n))
  for(i in 1:1000){
    mat[i, action] <- ten_bandits_instance(action)
  }
true_means <- ten_bandits_instance(1, T)$true_means
true_means_df <- data.frame(action = seq_len(n), value = true_means)
df <- as.data.frame(mat)%>%
  tidyr::pivot_longer(cols = tidyr::everything(), names_to = "action")
df$action <- forcats::fct_inorder(as.factor(df$action))
ggplot(df, aes(x = action, y = value)) +
  geom_violin() +
  geom_linerange() +
  geom_point(data = true_means_df)


## figure 2

#------- reward simple
mat <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat[i, ] <- bandit_agent( eps = 0.01, steps = 1000)$rewards

plot(colMeans(mat), type = "l", col = "red")

mat2 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat2[i, ] <- bandit_agent( eps = 0.1, steps = 1000)$rewards
lines(colMeans(mat2), type = "l", col = "blue")


mat3 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat3[i, ] <- bandit_agent( eps = 0, steps = 1000)$rewards
lines(colMeans(mat3), type = "l", col = "green")


#---action
mat2 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat2[i, ] <- bandit_agent( eps = 0.1, steps = 1000)$actions
plot(colMeans(mat2), type = "l", col = "blue", ylim = c(0, 1))


mat <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat[i, ] <- bandit_agent( eps = 0.01, steps = 1000)$actions
lines(colMeans(mat), type = "l", col = "red")


mat3 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat3[i, ] <- bandit_agent( eps = 0, steps = 1000)$actions 
lines(colMeans(mat3), type = "l", col = "green")


##figure 3

mat2 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat2[i, ] <- bandit_agent( eps = 0.1, steps = 1000, initial_action_value = 5, learning_function = learn_step_size)$actions
plot(colMeans(mat2), type = "l", col = "blue")

mat3 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat3[i, ] <- bandit_agent( eps = 0.1, steps = 1000, learning_function = learn_step_size)$actions 
lines(colMeans(mat3), type = "l", col = "red")




##figure 4

mat <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat[i, ] <- bandit_agent(eps = 0, steps = 1000, UCB_degree = 2)$rewards

plot(colMeans(mat), type = "l", col = "red")

mat2 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat2[i, ] <- bandit_agent( eps = 0.1, steps = 1000)$rewards
lines(colMeans(mat2), type = "l", col = "blue")



##figure 5

mat <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat[i, ] <- bandit_agent(step_size = 0.1, is_gradient = T, is_gradient_baseline = T, add_to_reward = 4)$actions
plot(colMeans(mat), type = "l", col = "red")

mat2 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat2[i, ] <- bandit_agent(step_size = 0.1, is_gradient = T, is_gradient_baseline = F, add_to_reward = 4)$actions
lines(colMeans(mat2), type = "l", col = "blue")

mat3 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat3[i, ] <- bandit_agent(step_size = 0.4, is_gradient = T, is_gradient_baseline = T, add_to_reward = 4)$actions
lines(colMeans(mat3), type = "l", col = "black")

mat4 <- matrix(nrow = 2000 , ncol = 1000)
for(i in 1:2000)
  mat4[i, ] <- bandit_agent(step_size = 0.4, is_gradient = T, is_gradient_baseline = F, add_to_reward = 4)$actions
lines(colMeans(mat4), type = "l", col = "green")