# day10 <- readLines("./inputs/10_input_exp2.txt")
day10 <- readLines("./inputs/10_input.txt")
day10 <- do.call("rbind", lapply(strsplit(day10, ""), as.numeric))

source("./code/day10_function.R")

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

# identify all trail heads
trailheads <- which(day10 == 0)
trailhead_scores <- vector("numeric", length(trailheads))

for (i in seq_along(trailheads)) {
  
  temp_paths <- list(trailheads[i])
  tops_reached <- vector("numeric")
  
  while (length(temp_paths) != 0) {

    temp_paths <- dijkstra(temp_paths, day10)

    top_idx <- day10[sapply(temp_paths, function(x) x[length(x)])] == 9

    if (any(top_idx)) {

      tops_reached <- unique(c(tops_reached, unname(sapply(temp_paths, function(x) x[length(x)])[top_idx])))
      temp_paths <- temp_paths[!top_idx]

    }

  }

  trailhead_scores[i] <- length(tops_reached)
  
}

sum(trailhead_scores)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

trailhead_scores <- vector("numeric", length(trailheads))

for (i in seq_along(trailheads)) {
  
  temp_paths <- list(trailheads[i])
  finished_paths <- 0
  
  while (length(temp_paths) != 0) {

    temp_paths <- dijkstra(temp_paths, day10)

    top_idx <- day10[sapply(temp_paths, function(x) x[length(x)])] == 9

    if (any(top_idx)) {

      finished_paths <- finished_paths + sum(top_idx)
      temp_paths <- temp_paths[!top_idx]

    }

  }

  trailhead_scores[i] <- finished_paths
  
}

sum(trailhead_scores)

Sys.time() - start
