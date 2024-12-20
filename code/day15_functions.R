move_robot <- function(pos, direction, area) {
  
  new_pos <- pos
  
  if (direction == "<") { # left
    new_pos[2] <- new_pos[2] - 1
  } else if (direction == ">") { # right
    new_pos[2] <- new_pos[2] + 1
  } else if (direction == "^") { # up
    new_pos[1] <- new_pos[1] - 1
  } else if (direction == "v") { # down
    new_pos[1] <- new_pos[1] + 1
  }
  
  if (area[new_pos] == "#") {
    
    new_pos <- pos
  
  } else if (area[new_pos] == "O") {
  
  # move boxes
  moved_boxes <- move_boxes(new_pos, direction, area)
  new_pos <- moved_boxes[[1]]
  area <- moved_boxes[[2]]
  
}

return(list(new_pos, area))

}

move_boxes <- function(new_pos, direction, area) {
  
  if (direction == "<") { # left
    
    look_ahead <- area[cbind(new_pos[1], 1:(new_pos[2] - 1))]
    next_wall <- max(which(look_ahead == "#"))
    next_free <- which(look_ahead == ".")
    next_free <- ifelse(length(next_free) == 0, NA, max(next_free))
    
    if (!(is.na(next_free) | next_wall > next_free)) {
      
      area[new_pos] <- "."
      area[cbind(new_pos[1], next_free)] <- "O"
      
    } else {
      
      new_pos[2] <- new_pos[2] + 1
      
    }
    
  } else if (direction == ">") { # right
    
    look_ahead <- area[cbind(new_pos[1], (new_pos[2] + 1):ncol(area))]
    next_wall <- min(which(look_ahead == "#"))
    next_free <- which(look_ahead == ".")
    next_free <- ifelse(length(next_free) == 0, NA, min(next_free))
    
    if (!(is.na(next_free) | next_wall < next_free)) {
      
      area[new_pos] <- "."
      area[cbind(new_pos[1], new_pos[2] + next_free)] <- "O"
      
    } else {
      
      new_pos[2] <- new_pos[2] - 1
      
    }
    
  } else if (direction == "^") { # up
    
    look_ahead <- area[cbind(1:(new_pos[1] - 1), new_pos[2])]
    next_wall <- max(which(look_ahead == "#"))
    next_free <- which(look_ahead == ".")
    next_free <- ifelse(length(next_free) == 0, NA, max(next_free))
    
    if (!(is.na(next_free) | next_wall > next_free)) {
      
      area[new_pos] <- "."
      area[cbind(next_free, new_pos[2])] <- "O"
      
    } else {
      
      new_pos[1] <- new_pos[1] + 1
      
    }
    
  } else if (direction == "v") { # down
    
    look_ahead <- area[cbind((new_pos[1] + 1):nrow(area), new_pos[2])]
    next_wall <- min(which(look_ahead == "#"))
    next_free <- which(look_ahead == ".")
    next_free <- ifelse(length(next_free) == 0, NA, min(next_free))
    
    if (!(is.na(next_free) | next_wall < next_free)) {
      
      area[new_pos] <- "."
      area[cbind(new_pos[1] + next_free, new_pos[2])] <- "O"
      
    } else {
      
      new_pos[1] <- new_pos[1] - 1
      
    }
    
  }

  return(list(new_pos, area))
  
}
