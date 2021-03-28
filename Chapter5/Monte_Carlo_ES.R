library(tidyverse)
library(reshape2)
library(patchwork)


resample <- function(x, ...)
  x[sample.int(length(x), ...)]


deal_card <- function(){
  card <- resample(1:13, 1) %>% 
    min(10)
  return(card)
}


is_bust <- function(player_sum){
  return(player_sum > 21)
}


exploring_episode <- function() {
  dealer_card <- resample(1:10, 1)
  has_ace <- resample(0:1, 1)
  player_sum <- resample(1:10, 1) + 11
  action <-  resample(c(F, T), 1)
  state <- c(dealer_card, player_sum, has_ace)
  # print(state)
  episode(state, action)
}

episode <- function(init_state = NULL, init_action = NULL){
  
  if(is.null(init_state)){
    #init the game if no 'init_state' specified
    dealer_card <- deal_card()
    player_card1 <- deal_card()
    player_card2 <- deal_card()
    has_ace <- player_card1 == 1 | player_card2 == 1
    player_sum <- player_card1 + player_card2 + ifelse(has_ace, 10, 0)
  } else {
    # for user input state:
    # init_state must be a vector of 3 (dealer_card, player_sum, has_ace)
    dealer_card <- init_state[[1]]
    player_sum <- init_state[[2]]
    has_ace <- init_state[[3]]
  }
  
  dealer_hidden_card <- deal_card()
  count <- 1
  episode_states <- list()
  
  
  while(TRUE){
    
    if(!is.null(init_action)){
  # 'init_action' must be 1(hit) or 0(stick)
      action <- init_action
      init_action <- NULL
    } else {
      action <- policy[dealer_card, player_sum, has_ace + 1]
    }
    
    episode_states[[count]] <-
      list(
        "dealer_card" = dealer_card,
        "player_sum" = player_sum,
        "has_ace" = has_ace,
        "action" = action
      )
    
    if(action == 0) ## 0 for stick
      break
    
    # drawing card
    new_card <- deal_card()
    player_sum <- player_sum + new_card
    if(!has_ace & new_card == 1){
      player_sum <- player_sum + 10
      has_ace <- TRUE
    }
    if(player_sum > 21 & has_ace){
      player_sum <- player_sum - 10
      has_ace <- FALSE
    } 
    
    if(is_bust(player_sum))
      break
    
    count <- count + 1
  }
  
  outcome <- outcome(dealer_card, dealer_hidden_card, player_sum)
  learn(outcome, episode_states)
  return(list("outcome" = outcome, "episode_states" = episode_states))
}




learn <- function(outcome, episode){
  for(state in episode){
    dealer_card <- state[["dealer_card"]]
    player_sum <- state[["player_sum"]]
    has_ace <- state[["has_ace"]]
    action <- state[["action"]] + 1
    ace <- has_ace + 1
    
    if(player_sum > 11){
      N[dealer_card, player_sum, ace, action] <<- N[dealer_card, player_sum, ace, action] + 1   
      Q[dealer_card, player_sum, ace, action] <<- Q[dealer_card, player_sum, ace, action] +
        (outcome - Q[dealer_card, player_sum, ace, action]) /  N[dealer_card, player_sum, ace, action]
      
      policy[dealer_card, player_sum, ace] <<- which.max(Q[dealer_card, player_sum, ace, ]) - 1
    }
  }
}


outcome <- function(dealer_card, dealer_hidden_card, player_sum){
  #dealer's turn
  dealer_ace <- dealer_card == 1 | dealer_hidden_card == 1
  dealer_sum <- dealer_card + dealer_hidden_card
  if(dealer_ace)
    dealer_sum <- dealer_sum + 10
  
  dealer_natural <- dealer_sum == 21
  
  if(dealer_natural & player_sum == 21)
    return(0)
  if(dealer_natural)
    return(-1)
  if(is_bust(player_sum))
    return(-1)
  
  while(dealer_sum < 17){
    new_card <- deal_card()
    dealer_sum <- dealer_sum + new_card
    
    if(!dealer_ace & new_card == 1){
      dealer_sum <- dealer_sum + 10
      dealer_ace <- TRUE
    }
    if(dealer_ace & dealer_sum > 21){
      dealer_sum <- dealer_sum - 10
      dealer_ace <- FALSE
    }
  }
  
  if(dealer_sum > 21)
    return(1)
  if(dealer_sum > player_sum)
    return(-1)
  if(dealer_sum == player_sum)
    return(0)
  
  return(1)
}

experiment <- function(num_episodes = 500000) {
  N <<- Q <<- array(0, dim = c(10, 21, 2, 2))
  policy <<- array(1, dim = c(10, 21, 2))
  policy[, 20:21, ] <<- 0

  for(i in seq_len(num_episodes)){
    exploring_episode()
  }
}


# Plotting figure

plot_fig5.2 <- function() {
  
  experiment(500000 * 3)
  
  P1 <- melt(policy[1:10, 11:21, 1]) %>%
    transmute(action = factor(value, labels = c("stick", "hit")),
              dealer_card = as.factor(Var1),
              player_sum = as.factor(Var2 + 10)) %>% 
    ggplot(aes(dealer_card, player_sum)) +
    geom_tile(aes(fill = action)) +
    labs(x = "dealer showing", y = "player sum", title = expression(pi*"* for no usable ace") )
  
  
  P2 <- melt(policy[1:10, 11:21, 2]) %>%
    transmute(action = factor(value, labels = c("stick", "hit")),
              dealer_card = as.factor(Var1),
              player_sum = as.factor(Var2 + 10)) %>% 
    ggplot(aes(dealer_card, player_sum)) +
    geom_tile(aes(fill = action)) +
    labs(x = "dealer showing", y = "player sum", title = expression(pi*"* for usable ace") )
  
  
  a <- Q[1:10, 12:21, 1, 1]
  b <- Q[1:10, 12:21, 1, 2]
  
  P3 <- map2_dbl(a, b, ~max(.x, .y)) %>% 
    matrix(nrow = 10, ncol = 10) %>% 
    melt() %>% 
    mutate(dealer_card = as.factor(Var1),
           player_sum = as.factor(Var2 + 12)) %>% 
    ggplot(aes(dealer_card, player_sum)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "dealer showing", y = "player sum", title = "V* for no usable ace")
  
  
  
  a <- Q[1:10, 12:21, 2, 1]
  b <- Q[1:10, 12:21, 2, 2]
  
  P4 <- map2_dbl(a, b, ~max(.x, .y)) %>% # collecting max values from both actions
    matrix(nrow = 10, ncol = 10) %>% 
    melt() %>% 
    mutate(dealer_card = as.factor(Var1),
           player_sum = as.factor(Var2 + 12)) %>% 
    ggplot(aes(dealer_card, player_sum)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "dealer showing", y = "player sum", title = "V* for usable ace")

  
  wrap_plots(P1, P2, P3, P4)
}


plot_fig5.2()

