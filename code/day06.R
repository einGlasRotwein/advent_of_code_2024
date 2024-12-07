# day6 <- readLines("./inputs/06_input_exp.txt")
day6 <- readLines("./inputs/06_input.txt")
day6 <- do.call("rbind", strsplit(day6, ""))

move_guard <- function(area, x, y, direction) {
  
  if (direction == "u") {
    
    match_obstacle <- which(area[1:x, y] == "#")
    
    if (length(match_obstacle) != 0) {
      
      new_row <- max(match_obstacle) + 1
      direction <- "r"
      
    } else {
      
      on_map <- FALSE
      new_row <- 1
      
    }

    visited <- data.frame(x = new_row:(x - 1), y = y, orientation = "u")
    x <- new_row
    
  } else if (direction == "r") {
    
    match_obstacle <- which(area[x, y:ncol(area)] == "#")
    
    if (length(match_obstacle) != 0) {
      
      new_col <- y + min(match_obstacle) - 2
      direction <- "d"
      
    } else {
      
      on_map <- FALSE
      new_col <- ncol(area)
      
    }
    
    visited <- data.frame(x = x, y = (y + 1):new_col, orientation = "r")
    y <- new_col
    
  } else if (direction == "d") {
    
    match_obstacle <- which(area[x:nrow(day6), y] == "#")
    
    if (length(match_obstacle) != 0) {
      
      new_row <- x + min(match_obstacle) - 2
      direction <- "l"
      
    } else {
      
      on_map <- FALSE
      new_row <- nrow(area)
      
    }
    
    visited <- data.frame(x = (x + 1):new_row, y = y, orientation = "d")
    x <- new_row
    
  } else if (direction == "l") {
    
    match_obstacle <- which(area[x, 1:y] == "#")
    
    if (length(match_obstacle) != 0) {
      
      new_col <- max(match_obstacle) + 1
      direction <- "u"
      
    } else {
      
      on_map <- FALSE
      new_col <- 1
      
    }
    
    visited <- data.frame(x = x, y = new_col:(y - 1), orientation = "l")
    visited$orientation <- "l"
    y <- new_col
    
  }

  return(list(x = x, y = y, direction = direction, visited = visited, on_map = on_map))
  
}

## PART 1 + 2 ----------------------------------------------------------------------------

# New idea: If I'm going up, and I turn right, can I hit an obstacle?
# If I'm going right, and I turn down, can I hit an obstacle?
# Etc.
# If I hit an obstacle, determine whether it leads back to a place I visited before 
# in the same direction (loop), or leads out of the matrix eventually.

temp_pos <- which(day6 == "^", arr.ind = TRUE)
temp_orientation <- "u"
path_track <- as.data.frame(temp_pos)
path_track$orientation <- temp_orientation
names(path_track) <- c("x", "y", "orientation")
on_map <- TRUE

while(on_map) {

  temp_move <- move_guard(day6, temp_pos[1], temp_pos[2], temp_orientation)
  temp_pos <- cbind(temp_move$x, temp_move$y)
  path_track <- rbind.data.frame(path_track, temp_move$visited)
  path_track <- unique(path_track)
  temp_orientation <- temp_move$direction
  on_map <- temp_move$on_map
  
}

# part 1
nrow(unique(path_track[c("x", "y")]))
