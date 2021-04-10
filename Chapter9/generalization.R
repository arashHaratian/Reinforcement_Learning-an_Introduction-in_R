library(tidyverse)
library(patchwork)


init_weights <- function(n = 100){
  w <<- vector("double", n)
}


f <- function(x, width, n = 100){
  sum <- 0
  for(i in 1:n)
    if(within_patch(x, i-1, width))
      sum <- sum + w[i]
  
  return(sum)
}


targetf <- function(x) {
  if(x >= 0.4 & x <= 0.6)
    return(1)
  return(0)
}


within_patch <- function(x, i, width, n = 100) {
  abs(x - (i / n)) <= (width/2/n)
}


train <- function(width, n = 100, alpha = 0.2){
  
  # vectorize (tidy) version:
  
  x <- 0.25 + runif(n, min = 0, max = 0.5)

  f_output <- map_dbl(x, f, width)
  target_output <- map_dbl(x, targetf)

  alpha_error <- ((alpha / width) * (target_output - f_output))
  within <- map2_lgl(x, 0:99, within_patch, width)
  w[which(within)] <<- w[which(within)] + alpha_error[which(within)]

  
  # for version:
  
  # for(j in 1:n){
  #   x <- 0.25 + runif(1, min = 0, max = 0.5)
  #   target_output <- targetf(x)
  #   f_output <- f(x)
  #   alpha_error <- ((alpha / width) * (target_output - f_output))
  #   if(within_patch(x, j-1, width))
  #     w[[j]] <<- w[[j]] + alpha_error
  # }
}



#plotting the figure 9.8

plot_fig9.8 <- function(){
  widths <- c(3, 9, 27)
  examples <- c(10 ,30 ,120 ,480 ,1920 ,7680)
  result <- list()
  range <- (100/4):(100 - (100/4))
  for(width in widths){
    
    total_exmaples <- 0
    init_weights()
    
    for(example in examples){
      
      total_exmaples <- total_exmaples + example
      print(total_exmaples)
      
      for(i in seq_len(example))
        train(width = width)
      
      index <- paste(width, total_exmaples, sep = "_")
      
      result[[index]] <- map_dbl(range, ~f(.x/100, width))
    }
  }
  
  desired_function <- map_dbl(range, ~targetf(.x/100))
  
  desired_function_line <- as.data.frame(desired_function) %>% 
    imap(~geom_line(aes(y = .x, color = .y)))
  
  low <- min(map_dbl(result, min))
  high <- max(map_dbl(result, max))
  
  plots <- result %>% 
    imap(~ggplot(data.frame("index" = 1:51), aes(x = index)) +
           geom_line(aes(y = .x)) +
           coord_cartesian(ylim = c(low, high)) + 
           labs(title = paste(str_split(.y, "_") %>% transpose %>% pluck(2), "steps"),
                y = paste(str_split(.y, "_") %>% transpose %>% pluck(1), "features"),
                x = "")
    )
  
  plots[[1]] <- plots[[1]] + desired_function_line
  plots[[7]] <- plots[[7]] + desired_function_line
  plots[[13]] <- plots[[13]] + desired_function_line
  
  wrap_plots(plots, nrow = 3, guides = "collect")
}


plot_fig9.8()
