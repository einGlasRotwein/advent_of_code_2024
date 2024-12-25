# thank you, 2022 Juli
# https://github.com/einGlasRotwein/advent_of_code_2022/blob/main/day12.R
dijkstra <- function(old_positions, area) {
  
  new_positions <- vector("numeric")
  
  for (i in seq_along(old_positions)) {
    temp_pos <- old_positions[i]
    
    # Get all neighbours of the current location.
    temp_neighbours <- c(
      temp_pos + 1, # down
      temp_pos - 1, # up
      temp_pos + nrow(area), # right
      temp_pos - nrow(area) # left
    )
    
    # If a node is at the top, it e.g. can't go up.
    # If a node is at the bottom (position == nrow), it can't go to the top
    # (position == nrow + 1)
    node_idx <- 
    c(
      temp_pos %% nrow(area) != 0, # bottom row (can't go down)
      (temp_pos - 1) %% nrow(area) != 0, # top row (can't go up)
      !(temp_pos - 1) %in% (length(area) - nrow(area) + 1):length(area), # right col (can't go right)
      !temp_pos %in% 1:nrow(area) # left col (can't go left)
    )
    
    temp_neighbours <- temp_neighbours[node_idx]
    
    # Remove all nodes that are out of bound
    temp_neighbours <- temp_neighbours[temp_neighbours %in% 1:length(area)]
    
    # Discard all neighbours that are blocked
    temp_neighbours <- temp_neighbours[area[temp_neighbours] != "#"]
    
    # If neighbours are empty (i.e. there are no more valid moves from 
    # this position), don't add to path.
    if (length(temp_neighbours) != 0) {
      
      # if a shorter path to the position already exists, don't add it
      improved_path <- 
      ifelse(area[temp_neighbours] == ".", Inf, area[temp_neighbours]) > 
      as.numeric(area[temp_pos]) + 1
      
      area[temp_neighbours[improved_path]] <- as.numeric(area[temp_pos]) + 1
      
      temp_neighbours <- temp_neighbours[improved_path]
      
      new_positions <- c(new_positions, temp_neighbours)
      
    }
  }
  
  return(list(new_positions, area))
}

# version for part 2 which also saves the full path
dijkstra_long <- function(old_positions, area) {
  
  new_positions <- vector("numeric")
  
  for (i in seq_along(old_positions)) {
    temp_pos <- old_positions[[i]][[length(old_positions[[i]])]]
    
    # Get all neighbours of the current location.
    temp_neighbours <- c(
      temp_pos + 1, # down
      temp_pos - 1, # up
      temp_pos + nrow(area), # right
      temp_pos - nrow(area) # left
    )
    
    # If a node is at the top, it e.g. can't go up.
    # If a node is at the bottom (position == nrow), it can't go to the top
    # (position == nrow + 1)
    node_idx <- 
    c(
      temp_pos %% nrow(area) != 0, # bottom row (can't go down)
      (temp_pos - 1) %% nrow(area) != 0, # top row (can't go up)
      !(temp_pos - 1) %in% (length(area) - nrow(area) + 1):length(area), # right col (can't go right)
      !temp_pos %in% 1:nrow(area) # left col (can't go left)
    )
    
    temp_neighbours <- temp_neighbours[node_idx]
    
    # Remove all nodes that are out of bound
    temp_neighbours <- temp_neighbours[temp_neighbours %in% 1:length(area)]
    
    # Discard all neighbours that are blocked
    temp_neighbours <- temp_neighbours[area[temp_neighbours] != "#"]
    
    # If neighbours are empty (i.e. there are no more valid moves from 
    # this position), don't add to path.
    if (length(temp_neighbours) != 0) {
      
      # if a shorter path to the position already exists, don't add it
      improved_path <- 
      ifelse(area[temp_neighbours] == ".", Inf, area[temp_neighbours]) > 
      as.numeric(area[temp_pos]) + 1
      
      area[temp_neighbours[improved_path]] <- as.numeric(area[temp_pos]) + 1
      
      temp_neighbours <- temp_neighbours[improved_path]
      
      if (length(temp_neighbours) != 0) {
        
        temp_continue <- 
        expand.grid(
          temp_pos,
          temp_neighbours
        )
        
        temp_continue <- split(temp_continue, seq(nrow(temp_continue)))
        temp_continue <- lapply(temp_continue, function(x) unname(unlist(x)))
        
        temp_add_paths <-
        lapply(temp_continue, function(x){
          c(old_positions[[i]], x[-1])
        })
        
        new_positions <- c(new_positions, temp_add_paths)
        
      }
      
    }
  }
  
  return(list(new_positions, area))
}