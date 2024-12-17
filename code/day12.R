day12 <- readLines("./inputs/12_input_exp3.txt")
# day12 <- readLines("./inputs/12_input.txt")
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

## PART 1 --------------------------------------------------------------------------------

plant_types <- unique(as.vector(day12))
plant_types <- plant_types[plant_types != "."]
clusters <- list()
perimeters <- vector("numeric")
plant_names <- vector("numeric")

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
    outside_fields <- arrayInd(unique(neighbours[day12[neighbours] != plant_type]), .dim = dim(day12))

    # identifiy sequences in every row/column
    # TO DO
    for (row in unique(outside_fields[ , 1])) {

      outside_fields[outside_fields[, 1] == row, ]
    }
    
  }
  
}

sum(sapply(clusters, length) * perimeters)
