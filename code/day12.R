day12 <- readLines("./inputs/12_input_exp3.txt")
day12 <- do.call("rbind", strsplit(day12, ""))

# to do: loop through plants
day12 == "I"

# take the first number and check which ones it is adjacent to
# take the numbers newly identified and check which numbers THEY are adjacent to, etc.
plants <- which(day12 == "I")

clusters <- list()

while (length(plants) != 0) {
  
  temp_plants <- plants[1]
  plants <- plants[-1]
  temp_cluster <- temp_plants
  
  while (length(temp_plants) != 0) {
    
    # positions at the end/beginning of rows are not neighbours of 
    # positions at the beginning/end of rows
    neighbours <- 
    c(
      ifelse(temp_plants %% nrow(day12) == 0, NA, temp_plants + 1), # down
      ifelse(temp_plants %% nrow(day12) == 1, NA, temp_plants - 1), # up
      ifelse(temp_plants %in% (nrow(day12) * (ncol(day12) - 1) + 1:nrow(day12)), NA, temp_plants + nrow(day12)), # right
      ifelse(temp_plants %in% 1:nrow(day12), NA, temp_plants - nrow(day12)) # left
    )
    
    temp_plants <- unique(neighbours[neighbours %in% plants])
    temp_plants <- temp_plants[!is.na(temp_plants)]
    temp_cluster <- c(temp_cluster, temp_plants)
    plants <- plants[!plants %in% neighbours]
    
  }

  clusters <- c(clusters, list(temp_cluster))
  
}

clusters
