# day8 <- readLines("./inputs/08_input_exp.txt")
day8 <- readLines("./inputs/08_input.txt")
day8 <- do.call("rbind", strsplit(day8, ""))

get_antinodes <- function(area, a1, a2) {

  x_diff <- a1[1] - a2[1]
  y_diff <- a1[2] - a2[2]

  n1 <- c(a1[1] + x_diff, a1[2] + y_diff)
  n2 <- c(a2[1] - x_diff, a2[2] - y_diff)

  output <- list()

  if (all(n1 > 0) & n1[1] <= nrow(area) & n1[2] <= ncol(area)) output <- c(output, list(n1))
  if (all(n2 > 0) & n2[1] <= nrow(area) & n2[2] <= ncol(area)) output <- c(output, list(n2))

  return(output)

}

# a1 <- c(5, 5)
# a2 <- c(3, 6)

get_antinodes2 <- function(area, a1, a2) {

  x_diff <- a1[1] - a2[1]
  y_diff <- a1[2] - a2[2]

  output <- list()

  n1 <- c(a1[1] + x_diff, a1[2] + y_diff)
  n1_within_bounds <- all(n1 > 0) & n1[1] <= nrow(area) & n1[2] <= ncol(area)

  if (n1_within_bounds) {
    output <- c(output, list(n1))
  }

  while(n1_within_bounds) {
    n1 <- c(n1[1] + x_diff, n1[2] + y_diff)
    n1_within_bounds <- all(n1 > 0) & n1[1] <= nrow(area) & n1[2] <= ncol(area)

    if (n1_within_bounds) {
      output <- c(output, list(n1))
    }
  }

  n2 <- c(a2[1] - x_diff, a2[2] - y_diff)
  n2_within_bounds <- all(n2 > 0) & n2[1] <= nrow(area) & n2[2] <= ncol(area)

  if (n2_within_bounds) {
    output <- c(output, list(n2))
  }

  while(n2_within_bounds) {
    n2 <- c(n2[1] - x_diff, n2[2] - y_diff)
    n2_within_bounds <- all(n2 > 0) & n2[1] <= nrow(area) & n2[2] <= ncol(area)

    if (n2_within_bounds) {
      output <- c(output, list(n2))
    }
  }

  output <- c(list(a1), list(a2), output)

  return(output)

}

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

frequencies <- unique(as.vector(day8))
frequencies <- frequencies[frequencies != "."]

antinodes <- list()

for (frq in frequencies) {

  antennas <- which(day8 == frq, arr.ind = TRUE)
  antenna_pairs <- combn(nrow(antennas), 2)

  for (j_pair in 1:ncol(antenna_pairs)) {

    a1 <- antennas[antenna_pairs[1, j_pair], ]
    a2 <- antennas[antenna_pairs[2, j_pair], ]

    antinodes <- c(antinodes, get_antinodes(day8, a1, a2))

  }

}

length(unique(antinodes))

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

antinodes <- list()

for (frq in frequencies) {

  antennas <- which(day8 == frq, arr.ind = TRUE)
  antenna_pairs <- combn(nrow(antennas), 2)

  for (j_pair in 1:ncol(antenna_pairs)) {

    a1 <- antennas[antenna_pairs[1, j_pair], ]
    a2 <- antennas[antenna_pairs[2, j_pair], ]

    antinodes <- c(antinodes, get_antinodes2(day8, a1, a2))

  }

}

length(unique(antinodes))

Sys.time() - start
