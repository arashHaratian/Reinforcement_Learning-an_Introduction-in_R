episode <- function(){
  actions <- vector("integer")
  while (TRUE) {
    action <- runif(1, 0, 1) < 0.5
    actions <- append(actions, action)
    if(action)
      return(list("reward" = 0, "actions" = actions))
    if(runif(1, 0, 1) < 0.1)
      return(list("reward" = 1, "actions" = actions))
  }
}


MC <- function(episodes = 100000){
  rewards <- vector("double", length = episodes)
  for(episode in seq_len(episodes)) {
    result <- episode()
    reward <- result[["reward"]]
    actions <- result[["actions"]]
    if(actions[[length(actions)]]){
      rho <- 0
    }else{
      rho <- 1 / 0.5 ^ length(actions)
    }
    reward <- reward * rho
    rewards[[episode]] <- reward
  }
  return(rewards)
}


runs <- function(num_runs = 10){
  estimations <- list()
  for(run in seq_len(num_runs)){
    rewards <- MC()
    estimations[[run]] <- cumsum(rewards)/seq_along(rewards)
  }
  return(estimations)
}


# plotting figure 5.4

plot_fig5.4 <- function(){
  estimations <- runs()
  estimations <- as.data.frame(estimations)
  num_of_episodes <- dim(estimations)[[1]]
  names(estimations) <- c(1:10)
  x_axis <- data.frame("episodes" = seq_len(num_of_episodes))
  
  lines <- imap(estimations, ~geom_line(aes(y = .x, color = .y)))
  
  ggplot(aes(x = episodes), data = x_axis) +
    lines +
    scale_x_log10() + 
    labs("x" = "episodes (log scale)", "y" = "MC estimate of V(s) with ordinary importance sampling") + 
    guides(color = FALSE)
}




plot_fig5.4()