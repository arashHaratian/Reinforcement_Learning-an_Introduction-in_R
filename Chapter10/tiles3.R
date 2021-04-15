# thanks for the idea of using env for hash table: https://www.r-bloggers.com/2019/01/hash-me-if-you-can/


initialize_IHT <- function(size_val){
  
  count <- 0
  IHT <- new.env(hash = TRUE, parent = emptyenv(), size = size_val)
  
  
  assign_hash <- function(coordinates, hash_table){
    if(!exists(coordinates, hash_table)){
      count <<- count + 1
      
      if (length(hash_table) >= size_val){
        message("'hash_table' is full, starting to allow collisions\n")
      }
      assign(coordinates, count, envir = hash_table)
    }
  }
  
  return(list("IHT" = IHT, "assign_hash_function" = assign_hash))
}


tiles <- function(IHT, assign_hash_function, num_tilings, state, action = NULL){
  
  qfloats <- floor(state * num_tilings)
  tiles <- rep(0, num_tilings)
  coordinates <- rep(0, length(state) + 1)
  
  for (tiling in seq_len(num_tilings)){
    tiling <- tiling - 1
    tilingX2 <- tiling * 2
    coordinates[[1]] <- tiling
    b <- tiling
    for (q in seq_along(qfloats)){
      coordinates[[q + 1]] <- (qfloats[[q]] + b) %/% num_tilings
      b <- b + tilingX2
    }
    coordinates <- append(coordinates, action)
    tiles[[tiling + 1]] <- .hash_coords(paste0(coordinates, collapse = ""), IHT, assign_hash_function)
  }
  
  return(tiles)
}


.hash_coords <- function(coordinates, IHT, assign_hash_function) {

  assign_hash_function(coordinates, IHT)
  return(.get_index(coordinates, IHT))
}


.get_index <- function(coordinates, IHT){
  return(get(coordinates, envir = IHT))
}

# testing:

# r <- initialize_IHT(1024)
# hash1 <- r[[1]]
# assign_hash1 <- r[[2]]

# tiles(hash1, assign_hash1, 8, state = c(3.6, 7.21))
# tiles(hash1, assign_hash1, 8, state = c(3.7, 7.21))
# tiles(hash1, assign_hash1, 8, state = c(4, 7))
# tiles(hash1, assign_hash1, 8, state = c(- 37.2, 7))
