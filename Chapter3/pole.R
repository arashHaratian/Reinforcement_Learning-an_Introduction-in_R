# this code is translated from C to R.
# Check the original C code:  http://incompleteideas.net/book/code/pole.c


resample <- function(x, ...)
  x[sample.int(length(x), ...)]



prob_push_right <- function(s){
  return(1 / (1 + exp(-max(-50, min(s, 50)))))
}



n_boxes <- 162
ALPHA <- 1000
BETA <- 0.5
GAMMA <- 0.95        
LAMBDAw <- 0.9        
LAMBDAv <- 0.8        

MAX_FAILURES <- 100
MAX_STEPS <- 100000


# main function in C code
main <- function(){
  x <- x_dot <- theta <- theta_dot <- 0
  w <- v <- e <- xbar <- vector("double", length = n_boxes)
  state <- c("x" = 0, "x_dot" = 0, "theta" = 0, "theta_dot" = 0)
  box <- get_box(state)
  
  failures <- 0
  step <- 0
  
  
  while(step < MAX_STEPS & failures < MAX_FAILURES){
    
    
    # step <- step + 1
    
    y <- runif(1) < prob_push_right(w[box])
    
    e[box] <- e[box] + (1.0 - LAMBDAw) * (y - 0.5)
    xbar[box] <- xbar[box] + (1.0 - LAMBDAv)
    
    oldp <- v[box]
    
    state <- cart_pole(y, state)
    
    box <- get_box(state)
    
    if(box < 0){
      
      failed <- TRUE
      failures <- failures + 1
      cat("Trial ", failures, " was ", step, " steps.\n")
      step <- 0 
      state <- c("x" = 0, "x_dot" = 0, "theta" = 0, "theta_dot" = 0)
      box <- get_box(state)
      r <- -1
      p <- 0
      
    } else {
      
      failed <- FALSE
      r <- 0
      p <- v[box] 
      step <- step + 1
    } 
    
    rhat <- r + GAMMA * p - oldp
    
    w <- w + ALPHA * rhat * e
    v <- v + BETA * rhat * xbar
    
    for(i in seq_len(n_boxes)){
      if (v[i] < -1.0)
        v[i] <- v[i]
    }
    
    if (failed){
      e <- rep(0, length(e))
      xbar <-  rep(0, length(xbar))
    } else{
      e <- e * LAMBDAw
      xbar <- xbar * LAMBDAv
    }
    
  }
  if (failures == MAX_FAILURES)
    cat("Pole not balanced. Stopping after ", failures, " failures.")
  else
    cat("Pole balanced successfully for at least ", step, " steps\n")

}


#--------------

GRAVITY <- 9.8
MASSCART <- 1.0
MASSPOLE <- 0.1
TOTAL_MASS <- MASSPOLE + MASSCART
LENGTH <- 0.5
POLEMASS_LENGTH <- MASSPOLE * LENGTH
FORCE_MAG <- 10.0
TAU <- 0.02
FOURTHIRDS <- 4/3


cart_pole <- function(action, state){
  
  x <- state[["x"]]
  x_dot <- state[["x_dot"]]
  theta <- state[["theta"]]
  theta_dot <- state[["theta_dot"]]
  
  force <- ifelse(action, FORCE_MAG, -FORCE_MAG)
  costheta <- cos(theta)
  sintheta <- sin(theta)
  
  temp <- (force + POLEMASS_LENGTH * theta_dot * theta_dot * sintheta) / TOTAL_MASS
  
  thetaacc <- (GRAVITY * sintheta - costheta * temp) / (LENGTH * (FOURTHIRDS - MASSPOLE * costheta * costheta / TOTAL_MASS))
  
  xacc <- temp - POLEMASS_LENGTH * thetaacc * costheta / TOTAL_MASS
  
  x <- x + TAU * x_dot
  x_dot <- x_dot + TAU * xacc
  theta <- theta + TAU * theta_dot
  theta_dot <- theta_dot + TAU * thetaacc
  
  state <- c("x" = x, "x_dot" = x_dot, "theta" = theta, "theta_dot" = theta_dot)
  
  
  return(state)
}


#--------------

one_degree <- pi / 180
six_degrees <- 6 * one_degree
twelve_degrees <- 12 * one_degree
fifty_degrees <- 50 * one_degree


get_box <- function(state) {
  x <- state[["x"]]
  x_dot <- state[["x_dot"]]
  theta <- state[["theta"]]
  theta_dot <- state[["theta_dot"]]
  
  box <- 0
  
  if (x < -2.4 | x > 2.4 | theta < -twelve_degrees | theta > twelve_degrees)
    return(-1)
  
  if (x < -0.8) {
    box <- 0
  } else if (x < 0.8) {
    box <- 1
  } else{
    box <- 2
  }
  
  if (x_dot < -0.5) {
    box <- box
  } else if (x_dot < 0.5) {
    box <- box + 3
  } else{
    box <- box + 6
  }
  
  if (theta < -six_degrees) {
    box <- box
  }else if (theta < -one_degree) {
    box <- box + 9
  } else if (theta < 0) {
    box <- box + 18
  } else if (theta < one_degree) {
    box <- box + 27
  } else if (theta < six_degrees) {
    box <- box + 36
  } else{
    box <- box + 45
  }
  
  if (theta_dot < -fifty_degrees) {
    box <- box
  } else if (theta_dot < fifty_degrees) {
    box <- box + 54
  }else{
    box <- box + 108
  }
  
  return(box + 1)
}
