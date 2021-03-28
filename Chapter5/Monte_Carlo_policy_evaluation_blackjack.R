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
  player_natrual <- player_sum == 21

  count <- 1
  episode_states <- list()
  
  # for user input action: 
  if(!is.null(init_action)){
    #'init_action' must be 1(hit) or 0(stick)
    
    if(init_action == 1) {
      
      episode_states[[count]] <-
        list(
          "dealer_card" = dealer_card,
          "player_sum" = player_sum,
          "has_ace" = has_ace
        )
      
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
      count <- count + 1
      
    } else if(init_action == 0) {
      break
    }
  }
  
  while(TRUE){
    
    episode_states[[count]] <-
      list(
        "dealer_card" = dealer_card,
        "player_sum" = player_sum,
        "has_ace" = has_ace
      )
    
    if(policy[dealer_card, player_sum, has_ace + 1] == 0) ## 0 for stick
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
  
  outcome <- outcome(dealer_card, dealer_hidden_card, player_sum, player_natrual)
  
  learn(outcome, episode_states)
  return(list("outcome" = outcome, "episode_states" = episode_states))
}


learn <- function(outcome, episode){
  for(state in episode){
    dealer_card <- state[["dealer_card"]]
    player_sum <- state[["player_sum"]]
    has_ace <- state[["has_ace"]]
    ace <- has_ace + 1
    
    if(player_sum > 11){
      N[dealer_card, player_sum, ace] <<- N[dealer_card, player_sum, ace] + 1   
      
      V[dealer_card, player_sum, ace] <<- V[dealer_card, player_sum, ace] +
        (outcome - V[dealer_card, player_sum, ace]) /  N[dealer_card, player_sum, ace]
    }
  }
}

  
outcome <- function(dealer_card, dealer_hidden_card, player_sum, is_player_natrual){
  
  dealer_ace <- dealer_card == 1 | dealer_hidden_card == 1
  dealer_sum <- dealer_card + dealer_hidden_card
  if(dealer_ace)
    dealer_sum <- dealer_sum + 10
  
  dealer_natural <- dealer_sum == 21
  
  if(dealer_natural & is_player_natrual)
    return(0)
  if(dealer_natural)
    return(-1)
  if(is_player_natrual)
    return(1)
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
  N <<- V <<- array(0, dim = c(10, 21, 2))
  policy <<- array(1, dim = c(10, 21, 2))
  policy[, 20:21, ] <<- 0
  
  for(i in seq_len(num_episodes)){
    episode()
  }
}




# plotting figure 5.1

plot_fig5.1 <- function(is_3d = FALSE){
  
  experiment(10000)
  
  p1 <- melt(V[, 12:21, 1]) %>% 
    mutate(Var2 = as.factor(Var2 + 11), Var1 = as.factor(Var1)) %>% 
    ggplot(aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    guides(fill = FALSE) +
    labs(x = "dealer showing", y = "player sum", title = "no usable ace (10000 episodes)")
  
  
  p2 <- melt(V[, 12:21, 2]) %>% 
    mutate(Var2 = as.factor(Var2 + 11), Var1 = as.factor(Var1)) %>% 
    ggplot(aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    labs(x = "dealer showing", y = "player sum", title = "usable ace (10000 episodes)")

  
  experiment(500000)

  p3 <- melt(V[, 12:21, 1]) %>% 
    mutate(Var2 = as.factor(Var2 + 11), Var1 = as.factor(Var1)) %>% 
    ggplot(aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    guides(fill = FALSE) +
    labs(x = "dealer showing", y = "player sum", title = "no usable ace (500000 episodes)")
  
  p4 <- melt(V[, 12:21, 2]) %>% 
    mutate(Var2 = as.factor(Var2 + 11), Var1 = as.factor(Var1)) %>% 
    ggplot(aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "green") +
    guides(fill = FALSE) +
    labs(x = "dealer showing", y = "player sum",  title = "usable ace (500000 episodes)")
  
  wrap_plots(p1, p2, p3, p4, ncol = 2, guides = "collect")
}


# plot_fig5.1_3d <- function(){
# library(plotly)  # for 3d plots
###----------- this function will work but not so good

#   experiment(10000)
#   
#     p1 <- plot_ly(z = V[, 12:21, 1], scene='scene1') %>%
#       add_surface() %>%
#       layout(title = "no usable ace (10000 episodes)")
# 
#     p2 <- plot_ly(z = V[, 12:21, 2], scene='scene2') %>%
#       add_surface() %>%
#       layout(showlegend = F, title = "usable ace (10000 episodes)")
# 
#   experiment(500000)
#   
#     p3 <- plot_ly(z = V[, 12:21, 1], scene='scene3') %>%
#       add_surface() %>%
#       layout(showlegend = F, title = "no usable ace (500000 episodes)")
# 
#     p4 <- plot_ly(z = V[, 12:21, 2], scene='scene4') %>%
#       add_surface() %>%
#       layout(showlegend = F, title = "usable ace (500000 episodes)")
# 
#     subplot(p1, p2, p3, p4, nrows = 2)
# }




plot_fig5.1()

