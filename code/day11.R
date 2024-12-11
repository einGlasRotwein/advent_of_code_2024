# day11 <- "125 17"
day11 <- readLines("./inputs/11_input.txt")
day11 <- as.numeric(unlist(strsplit(day11, " ")))

options(scipen = 999)

blink <- function(stone_table) {
  
  n_digits <- nchar(names(stone_table))
  old_stones <- as.numeric(names(stone_table))
  
  # names
  stone_names <- 
  as.numeric(
    c(
      # is 0
      1,
      # even amount of digits
      substr(old_stones[n_digits %% 2 == 0], 1, nchar(old_stones[n_digits %% 2 == 0]) / 2),
      substr(old_stones[n_digits %% 2 == 0], (nchar(old_stones[n_digits %% 2 == 0]) / 2) + 1, nchar(old_stones[n_digits %% 2 == 0])),
      # the rest
      old_stones[n_digits %% 2 != 0 & old_stones != 0] * 2024
    )
  )
  
  # counts
  # is 0
  stone_counts <- 
  c(
    ifelse(!is.na(stone_table["0"]), stone_table["0"], 0), # is 0
    stone_table[n_digits %% 2 == 0], # split even number of digits
    stone_table[n_digits %% 2 == 0],
    stone_table[n_digits %% 2 != 0 & old_stones != 0] # multiply with 2024
  )

  stone_names <- stone_names[stone_counts != 0]
  stone_counts <- stone_counts[stone_counts != 0]

  # sum together the same stones that were created during this step, e.g., 
  # 4 x 2 and 10 x 2 = 14 x 2
  merged_stones <- by(stone_counts, stone_names, sum)

  stone_counts <- as.vector(merged_stones)
  names(stone_counts) <- names(merged_stones)
  
  return(stone_counts)
  
}

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

stone_table <- rep(1, length(day11))
names(stone_table) <- day11

n_blinks <- 25

for (i in 1:n_blinks) {

  stone_table <- blink(stone_table)

}

sum(stone_table)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

stone_table <- rep(1, length(day11))
names(stone_table) <- day11

n_blinks <- 75

for (i in 1:n_blinks) {

  stone_table <- blink(stone_table)

}

sum(stone_table)

Sys.time() - start
