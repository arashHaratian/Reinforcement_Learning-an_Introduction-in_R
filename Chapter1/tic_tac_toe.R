# tic tac toe
#TODO: make it an shiny app
#report bug in lisp version:  (defun run) call (init)

library(purrr)

resample <- function(x, ...)
  x[sample.int(length(x), ...)]

num_rows <- 3L
num_cols <- 3L

# alpha <- 0.5
# eps <- 0.01

player_one <- 1L  #(x)
player_two <- -1L #(o)

# initializing the game
initialize <- function(init_state = 0L) {
  board <- matrix(init_state, num_rows, num_cols)
  return(board)
} ## board <- initialize(c(1,1,0,0,1,0,0,0,1))


# initializing the value table
initialize_state_value <- function(init_val = 0) {
  value_table <- rep(init_val, 512 * 512)
  value_table[[1]] <- 0.5
  return(value_table)
} ## value_table <- initialize_state_value(NA)


# hashing the board state
state_index <- function(board) {
  power_of_2 <- matrix(2 ^ (0:8), num_rows, num_cols) ## check this in more detail
  state_hash <- sum(power_of_2[board == player_one]) + (512 * sum(power_of_2[board == player_two])) + 1
  
  return(state_hash)
} ## state_index(board)


# returns the reward
reward <- function(board) {
  if (any(colSums(board) == 3L) | any(rowSums(board) == 3L) |
      any(sum(diag(board)) == 3L) |
      any(sum(diag(board[nrow(board):1, ])) == 3L)) {
    return(1)
    
  }
  else if (any(colSums(board) == -3L) | any(rowSums(board) == -3L) |
           any(sum(diag(board)) == -3L) |
           any(sum(diag(board[nrow(board):1, ])) == -3L)) {
    return(0)
  } else if (all(board != 0)) {
    return(0)
  }
  return(0.5)
}  ## reward(board)

# check for end of the game
is_done <- function(board) {
  if (any(colSums(board) == 3L) | any(rowSums(board) == 3L) |
      any(sum(diag(board)) == 3L) |
      any(sum(diag(board[nrow(board):1, ])) == 3L)) {
    return(TRUE) ## for player one
  }
  if (any(colSums(board) == -3L) | any(rowSums(board) == -3L) |
      any(sum(diag(board)) == -3L) |
      any(sum(diag(board[nrow(board):1, ])) == -3L)) {
    return(TRUE)  ## for player two
  } else if (all(board != 0)) {
    return(TRUE)  ## tie
  }
  return(FALSE) ## still games goes on!
}## is_done(board)

# getting and setting value in value table
value_get <- function(value_table, index) {
  return(value_table[[index]])
} ## value_get(value_table, state_index(board))

value_set <- function(value_table, index, value) {
  value_table[[index]] <- value
  invisible(value_table)
} ## value_table <- value_set(value_table, state_index(board), 0.13)

# next possible moves (open positions)
possible_moves <- function(board) {
  return(which(board == 0L))
} ## possible_moves(board)


next_move <- function(board, player, epsilon) {
  legal_moves <- possible_moves(board)
  if (runif(1) > epsilon) {
    # best move and also update the value of that state
    
    # iterating over 'legal_moves' and gathering their values in the 'value_table'
    values_of_legal_moves <-
      #first collecting the next possible states
      purrr::map(
        legal_moves,
        .f = next_state,
        board = board,
        player = player
      ) %>%
      # then calculating their indexes
      map_dbl(.f = state_index) %>%
      # finally getting their values
      map_dbl(.f = value_get, value_table = value_table)
    best_value <- which.max(values_of_legal_moves)
    list <- list("move" = legal_moves[best_value], "is_greedy" = T)
    return(list)
  } else {
    # exploratory move
    list <- list("move" = resample(legal_moves, 1),
                 "is_greedy" = F)
    return(list)
  }
}

# taking action in the board
next_state <- function(board, player, move){
  if (player != 1L & player != -1L)
    ## checking for correct player(+1 for player one and -1 for player two)
    stop("'player' argument in 'next_state()' must be either +1 or -1")
  
  legal_moves <- possible_moves(board)
  
  if(all(legal_moves != move))
    stop("wrong move\n choose a move from 'possible_moves()'")

  board[[move]] <- player
  val <- reward(board)
  index <- state_index(board)

  if (is.na(value_get(value_table, index)))
    value_table <<- value_set(value_table, index, val)
    invisible(board)
}

# print the board
print_board <- function(board) {
  board[board == 1L] <- "X"
  board[board == -1L] <- "O"
  board[board == 0L] <- "--"
  print(board)
} ## print_board(board)


# update the value of the states (update the 'value_table')
value_update <- function(previous_state_index, current_state_index, alpha) {
  previous_state_value <- value_get(value_table, previous_state_index)
  if(is.na(previous_state_value))
    stop("na value")
  current_state_value <- value_get(value_table, current_state_index)
  new_value <- previous_state_value + (alpha * (current_state_value - previous_state_value)) ##the update rule
  # cat("\nvalue: ", new_value, "old_val: ", previous_state_value, "current_s_val: ", current_state_value, "\n")
  value_table <- value_set(value_table, index = previous_state_index, value = new_value)
  invisible(value_table)
}


# play a full game (player_one(X) will learn and player_two(O) will act randomly)
game <- function(eps = 0.01, alpha = 0.5, verbose = F) {
  # message("starting a new game...\n")
  board <- initialize()
  if (verbose)
    print_board(board)
  while (TRUE) {
    old_state_index <- state_index(board)
    player_two_move <- next_move(board, player_two, epsilon = 1)  #epsilon sets to 1 to act randomly
    board <- next_state(board, player = player_two, move = player_two_move$move) #player_two(O) move
    if (is_done(board)) {
      if (verbose)
        print_board(board)
      
      current_state_index <- state_index(board)
      value_table <<- value_update(old_state_index, current_state_index, alpha)
      return(value_get(value_table, current_state_index))
    }
    
    player_one_move <- next_move(board, player = player_one, epsilon = eps)
    board <- next_state(board, player = player_one, move = player_one_move$move)
    if(player_one_move$is_greedy){
      current_state <- state_index(board)
      value_table <<- value_update(old_state_index, current_state, alpha)
    }
    if (verbose)
      print_board(board)
    if (is_done(board)) {
      index <- state_index(board)
      return(value_get(value_table, index))
    }
  }
}


run <- function(num_bins = 40, bin_size = 100, ...){
  value_table <<- initialize_state_value(NA)
  avg <- vector("double", num_bins)
  for(i in seq_len(num_bins)){
    temp <- vector("double", bin_size)
    for(j in seq_len(bin_size))
      temp[[j]] <- game(...)
    avg[[i]] <- print(mean(temp))
  }
  invisible(avg)
}# run()


runs <- function(num_runs = 10, num_bins = 40, bin_size = 100, ...) {
  ##CAUTION: you should make a global value_table (outside the function)
  mat <- matrix(nrow = num_runs, ncol = num_bins)
  for(i in seq_len(num_runs))
    mat[i, ] <- run(num_bins, bin_size, ...)
  
  print(colMeans(mat))
  invisible(mat)
} #runs()



## using 'game()':
value_table <- initialize_state_value(NA)
game(verbose = T)

## using 'run()'
result_run <- run(40, 100, eps = 0.01, alpha = 0.5)
plot(result_run, ty= "l")

## using 'runs()'
result_runs <- runs(10, 100, 100, eps = 0.01, alpha = 0.5)
plot(result_run, ty= "l")
