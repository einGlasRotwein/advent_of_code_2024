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

    visited <- data.frame(x = x:new_row, y = y, orientation = "u")[-1, ] # exclude original position
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
    
    visited <- data.frame(x = x, y = y:new_col, orientation = "r")[-1, ]
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
    
    visited <- data.frame(x = x:new_row, y = y, orientation = "d")[-1, ]
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
    
    visited <- data.frame(x = x, y = y:new_col, orientation = "l")[-1, ]
    y <- new_col
    
  }

  return(list(x = x, y = y, direction = direction, visited = visited, on_map = on_map))
  
}

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

temp_pos <- start_pos <- which(day6 == "^", arr.ind = TRUE)
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

nrow(unique(path_track[c("x", "y")]))

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

# Fuck this. On my 7th attempt to solve this, I just use brute force. Throw in an obstacle 
# for each position on the path, and see whether I run into a loop.
# Apparently, we need to start at the beginning for every obstacle, because it might 
# disrupt the part at an even earlier point ...

start <- Sys.time()

new_obstacles <- data.frame(matrix(nrow = 0, ncol = 2))
names(new_obstacles) <- c("x", "y")

# to do: test what's wrong in i = 14

pb <- txtProgressBar(min = 0, max = nrow(path_track), initial = 0, style = 3) 

for (i in 2:nrow(path_track)) { # exclude start position
  
  temp_pos <- data.frame(x = start_pos[1], y = start_pos[2], orientation = "u") # always go back to start
  
  # place obstacle
  temp_area <- day6
  temp_area[cbind(path_track$x[i], path_track$y[i])] <- "#"
  
  # start from previous position and determine whether we run into a loop
  temp_orientation <- temp_pos$orientation
  on_map <- TRUE

  temp_path_track <- temp_pos
  
  while(on_map) {
    
    temp_move <- move_guard(temp_area, temp_pos$x, temp_pos$y, temp_orientation)
    temp_pos <- data.frame(x = temp_move$x, y = temp_move$y, direction = temp_move$direction)
    temp_path_track <- rbind.data.frame(temp_path_track, unique(temp_move$visited))
    
    if (nrow(unique(temp_path_track)) != nrow(temp_path_track)) {
      
      # if it's a loop, save obstacle position
      new_obstacles <- 
      rbind.data.frame(new_obstacles, data.frame(x = path_track$x[i], y = path_track$y[i]))
      on_map <- FALSE # and abort path
      
    } else {

      temp_orientation <- temp_move$direction
      on_map <- temp_move$on_map

    }
    
  }

  setTxtProgressBar(pb, i)
  
}

close(pb)

nrow(unique(new_obstacles))

Sys.time() - start
