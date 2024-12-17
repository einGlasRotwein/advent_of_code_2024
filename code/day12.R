# day12 <- readLines("./inputs/12_input_exp2.txt")
day12 <- readLines("./inputs/12_input.txt")
day12 <- do.call("rbind", strsplit(day12, ""))

# add padding so I won't have to deal with NAs
day12 <- cbind(rep(".", nrow(day12)), day12, rep(".", nrow(day12)))
day12 <- rbind(rep(".", ncol(day12)), day12, rep(".", ncol(day12)))

get_neighbours <- function(positions, area) {
  
  # positions at the end/beginning of rows are not neighbours of 
  # positions at the beginning/end of rows
  neighbours <- 
  c(
    ifelse(positions %% nrow(area) == 0, NA, positions + 1), # down
    ifelse(positions %% nrow(area) == 1, NA, positions - 1), # up
    ifelse(positions %in% (nrow(area) * (ncol(area) - 1) + 1:nrow(area)), NA, positions + nrow(area)), # right
    ifelse(positions %in% 1:nrow(area), NA, positions - nrow(area)) # left
  )
  
  return(neighbours)
  
}

## PART 1 and 2 --------------------------------------------------------------------------

start <- Sys.time()

plant_types <- unique(as.vector(day12))
plant_types <- plant_types[plant_types != "."]
clusters <- list()
perimeters <- vector("numeric")
plant_names <- vector("numeric")
n_sides <- vector("numeric")

for (plant_type in plant_types) {
  
  # take the first number and check which ones it is adjacent to
  # take the numbers newly identified and check which numbers THEY are adjacent to, etc.
  plants <- which(day12 == plant_type)
  
  while (length(plants) != 0) {
    
    temp_plants <- plants[1]
    plants <- plants[-1]
    temp_cluster <- temp_plants
    
    while (length(temp_plants) != 0) {
      
      # positions at the end/beginning of rows are not neighbours of 
      # positions at the beginning/end of rows
      neighbours <- get_neighbours(temp_plants, day12)
      
      temp_plants <- unique(neighbours[neighbours %in% plants])
      temp_plants <- temp_plants[!is.na(temp_plants)]
      temp_cluster <- c(temp_cluster, temp_plants)
      plants <- plants[!plants %in% neighbours]
      
    }
    
    ## PART 1
    clusters <- c(clusters, list(temp_cluster))
    
    # perimeters: take all neighbours of temp cluster and check how many of 
    # them are not the current plant type; that should give us the length of 
    # the borders
    # when the same tile is a neighbour of several plants, it counts twice
    
    neighbours <- get_neighbours(temp_cluster, day12)
    
    perimeters <- c(perimeters, sum(day12[neighbours] != plant_type))
    plant_names <- c(plant_names, plant_type) # needed for part 2
    
    ## PART 2
    temp_map <- day12
    temp_map[-temp_cluster] <- "."

    outside_fields <- arrayInd(unique(neighbours[temp_map[neighbours] != plant_type]), .dim = dim(temp_map))
    side_counter <- 0
    
    # identifiy sequences in every row
    for (row in unique(outside_fields[ , 1])) {
      
      temp_fields <- matrix(outside_fields[outside_fields[, 1] == row, ], ncol = 2)
      
      # top side: only consider fields with plant below them - 
      # THAT BELONG TO THE SAME CLUSTER!
      # This is why we are working with a temp_area that only contains the 
      # current cluster and otherwise .
      if (row != nrow(temp_map)) {
        
        top <- 
        matrix(
          temp_fields[
            temp_map[cbind(row + 1, temp_fields[ , 2])] == plant_type,
          ],
          ncol = 2
        )
        
        top <- matrix(top[order(top[ , 2]), ], ncol = 2) # fuck matric indexing
        
        if (nrow(top) != 0) {
          
          diffs <- diff(top[ , 2])
          
          if (all(diffs == 1) | length(diffs) == 0) {
            side_counter <- side_counter + 1
          } else {
            side_counter <- side_counter + sum(diff(top[ , 2]) > 1) + 1
          }
          
        }
        
      }
      
      if (row != 1) {
        
        # bottom side: only consider fields with plant above them
        bottom <- 
        matrix(
          temp_fields[
            temp_map[cbind(row - 1, temp_fields[ ,2])] == plant_type,
          ],
          ncol = 2
        )
        
        bottom <- matrix(bottom[order(bottom[ , 2]), ], ncol = 2)
        
        if (nrow(bottom) != 0) {
          
          diffs <- diff(bottom[ , 2])
          
          if (all(diffs == 1) | length(diffs) == 0) {
            side_counter <- side_counter + 1
          } else {
            side_counter <- side_counter + sum(diff(bottom[ , 2]) > 1) + 1
          }
          
        }
        
      }
      
    }
    
    # identifiy sequences in every column
    for (col in unique(outside_fields[ , 2])) {
      
      temp_fields <- matrix(outside_fields[outside_fields[, 2] == col, ], ncol = 2)
      
      if (col != ncol(day12)) {
        
        # left side: only consider fields with plant on the right
        left <- 
        matrix(
          temp_fields[
            temp_map[cbind(temp_fields[ , 1], col + 1)] == plant_type,
          ], ncol = 2
        )
        
        left <- matrix(left[order(left[ , 1]), ], ncol = 2)
        
        if (nrow(left) != 0) {
          
          diffs <- diff(left[ , 1])
          
          if (all(diffs == 1) | length(diffs) == 0) {
            side_counter <- side_counter + 1
          } else {
            side_counter <- side_counter + sum(diff(left[ , 1]) > 1) + 1
          }
          
        }
        
      }
      
      if (col != 1) {
        
        # right side: only consider fields with plant on the left
        right <- 
        matrix(
          temp_fields[
            temp_map[cbind(temp_fields[ , 1], col - 1)] == plant_type,
          ],
          ncol = 2
        )
        
        right <- matrix(right[order(right[ , 1]), ], ncol = 2)
        
        if (nrow(right) != 0) {
          
          diffs <- diff(right[ , 1])
          
          if (all(diffs == 1) | length(diffs) == 0) {
            side_counter <- side_counter + 1
          } else {
            side_counter <- side_counter + sum(diff(right[ , 1]) > 1) + 1
          }
          
        }
        
      }
      
    }
    
    n_sides <- c(n_sides, side_counter)
    
  }
  
}

sum(sapply(clusters, length) * perimeters) # part 1
sum(sapply(clusters, length) * n_sides) # part 2

Sys.time() - start
