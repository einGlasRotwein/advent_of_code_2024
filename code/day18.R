# day18 <- readLines("./inputs/18_input_exp.txt")
day18 <- readLines("./inputs/18_input.txt")

bytes <- do.call("rbind", lapply(strsplit(day18, ","), as.numeric))
bytes <- bytes + 1 # start index at 1

# example
# n_rows <- 7
# n_cols <- 7
# n_bytes <- 12

# actual input
n_rows <- 71
n_cols <- 71
n_bytes <- 1024

source("./code/day18_function.R")

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

# set obstacles
grid <- matrix(".", nrow = n_rows, ncol = n_cols)
grid[cbind(bytes[1:n_bytes, ])] <- "#"

# find shortest path
positions <- 1
grid[1] <- 0

while(grid[length(grid)] == ".") {
  
  temp_move <- dijkstra(positions, grid)
  positions <- temp_move[[1]]
  grid <- temp_move[[2]]
  
}

as.numeric(grid[length(grid)])

Sys.time() - start

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

positions <- list()

for (i in n_bytes:nrow(bytes)) {
  
  grid <- matrix(".", nrow = n_rows, ncol = n_cols)
  grid[cbind(bytes[1:i, ])] <- "#"
  
  # re-calculate path only if new byte disturbes previous path
  if (length(positions) != 0) {
    
    winning_path <- positions[sapply(positions, function(x) x[length(x)]) == length(grid)]
    
    if (length(winning_path) != 1) stop("More than 1 path!")
    
    winning_path <- unlist(winning_path)
    names(winning_path) <- NULL
    
    if (all(grid[winning_path] == ".")) {
      next
    }
  }
  
  positions <- list(1)
  grid[1] <- 0
  
  while(length(positions) != 0) {
    
    temp_move <- dijkstra_long(positions, grid)
    positions <- temp_move[[1]]
    grid <- temp_move[[2]]
    
    if (grid[length(grid)] != ".") break
    
  }
  
  if (grid[length(grid)] == ".") break
  
}

paste(bytes[i, ] - 1, collapse = ",")

Sys.time() - start

