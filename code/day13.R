day13 <- readLines("./inputs/13_input_exp.txt")
# day13 <- readLines("./inputs/13_input.txt")

day13 <- split(day13, cumsum(day13 == ""))
day13 <- lapply(day13, function(x) x[x != ""])

btn_a <- sapply(day13, `[[`, 1)
btn_b <- sapply(day13, `[[`, 2)
prize <- sapply(day13, `[[`, 3)

a_x <- as.numeric(gsub(".*X\\+(\\d+),.*", "\\1", btn_a))
a_y <- as.numeric(gsub(".*Y\\+(\\d+).*", "\\1", btn_a))
b_x <- as.numeric(gsub(".*X\\+(\\d+),.*", "\\1", btn_b))
b_y <- as.numeric(gsub(".*Y\\+(\\d+).*", "\\1", btn_b))
prize_x <- as.numeric(gsub(".*X\\=(\\d+),.*", "\\1", prize))
prize_y <- as.numeric(gsub(".*Y\\=(\\d+).*", "\\1", prize))

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

n_tokens <- vector("numeric", length(a_x))

max_presses <- 100

for (i in seq_along(a_x)) {

  # A
  a_presses <- 1:max_presses
  dist_a_x <- a_x[i] * a_presses
  dist_a_x <- dist_a_x[dist_a_x <= prize_x[i]]

  dist_a_y <- a_y[i] * a_presses
  dist_a_x <- dist_a_x[dist_a_y <= prize_y[i]]
  dist_a_y <- dist_a_y[dist_a_y <= prize_y[i]]

  # B
  remaining_x <- prize_x[i] - dist_a_x
  remaining_y <- prize_y[i] - dist_a_y

  possible_b_x <- remaining_x[floor(remaining_x / b_x[i]) == remaining_x / b_x[i]]
  possible_b_y <- remaining_y[floor(remaining_y / b_y[i]) == remaining_y / b_y[i]]

  n_b <- (possible_b_y / b_y[i])[(possible_b_y / b_y[i]) %in% (possible_b_x / b_x[i])]

  move_bx <- b_x[i] * n_b
  move_by <- b_y[i] * n_b

  # caculate back corresponding x values
  n_a_x <- (prize_x[i] - move_bx) / a_x[i]
  n_a_y <- (prize_y[i] - move_by) / a_y[i]

  n_a <- n_a_y[n_a_y == n_a_x]
  n_b <- n_b[n_a_y == n_a_x]

  # calculate sum(s)
  if (length(n_a) != 0 | length(n_b) != 0) n_tokens[i] <- min(n_a * 3 + n_b)

}

sum(n_tokens, na.rm = TRUE)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

# part 2 needs math, which I don't know :-)
