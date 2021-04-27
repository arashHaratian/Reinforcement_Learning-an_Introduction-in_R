library(tidyverse)
library(reshape2)



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
  count <- 1
  episode_states <- list()
  
  
  while(TRUE){
    
    if(!is.null(init_action)){
      # 'init_action' must be 1(hit) or 0(stick)
      action <- init_action
      init_action <- NULL
    } else {
      action <- rbinom(1, 1, 0.5) # behavior policy
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
  # learn(outcome, episode_states)
  return(list("outcome" = outcome, "episode_states" = episode_states))
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


monte_carlo_off_policy <- function(num_episodes){
  
  rhos <- vector("double", num_episodes)
  outcomes <- vector("double", num_episodes)
  
  for(i in seq_len(num_episodes)){
    
    numerator <- denominator <- 1
    
    result <- episode(c(2, 13, T))
    
    outcome <- result[["outcome"]]
    states <- result[["episode_states"]]
    
    for(state in states){
      dealer_card <- state[["dealer_card"]]
      player_sum <- state[["player_sum"]]
      has_ace <- state[["has_ace"]]
      action <- state[["action"]] + 1
      ace <- has_ace + 1
      
      if(action == policy[dealer_card, player_sum, ace]){
        denominator <- denominator * 0.5
      } else{
        numerator <- 0
        break
      }
    }
    
    rho <- numerator / denominator
    rhos[i] <- rho
    outcomes[i] <- outcome
  }
  
  weighted_outcomes <- cumsum(rhos * outcomes)
  rhos <- cumsum(rhos) #TODO
  
  ordinary_sampling <- weighted_outcomes / seq_len(num_episodes)
  weighted_sampling <- ifelse(rhos != 0,  weighted_outcomes / rhos, 0)
  
  return(list("weighted_sampling" = weighted_sampling, "ordinary_sampling" = ordinary_sampling))
}


experiment <- function(runs = 100){
  policy <<- array(1, dim = c(10, 21, 2))
  policy[, 20:21, ] <<- 0
  ordinary_mse <- weighted_mse <- vector("double", 10000)
  
  for(i in seq_len(runs)){
    result <- monte_carlo_off_policy(10000)
    
    ordinary_sampling <- result[["ordinary_sampling"]]
    weighted_sampling <- result[["weighted_sampling"]]
    
    ordinary_mse <- ordinary_mse + ((ordinary_sampling - (-0.27726)) ^ 2)
    weighted_mse <- weighted_mse + ((weighted_sampling - (-0.27726)) ^ 2)
  }

  
  ordinary_mse <- ordinary_mse / runs
  weighted_mse <- weighted_mse/ runs
  
  return(list("ordinary_mse" = ordinary_mse, "weighted_mse" = weighted_mse))
}


# plotting the figure

plot_fig5.3 <- function(){
  mses <- experiment(100)
  
  ordinary_mse <- mses[["ordinary_mse"]]
  weighted_mse <- mses[["weighted_mse"]]
  
  df <- data.frame(
    "ordinary_mse" = ordinary_mse,
    "weighted_mse" = weighted_mse,
    "episodes" = seq_len(length(ordinary_mse))
  )
  
  # plot <- 
    ggplot(df, aes(x = episodes)) +
    geom_line(aes(y = ordinary_mse, color = "ordinary_mse")) +
    geom_line(aes(y = weighted_mse, color = "weighted_mse")) +
    scale_x_log10() +
    labs(x = "episodes (log scale)", y = "mean square error (average over 100 runs)")
  
  return(plot)
}



# plot_fig5.3()
