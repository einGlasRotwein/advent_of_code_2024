# day13 <- readLines("./inputs/13_input_exp.txt")
day13 <- readLines("./inputs/13_input.txt")

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

options(scipen = 999)
tolerance <- 0.001 # floating point precision :-)))

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

n_tokens <- vector("numeric", length(a_x))

max_presses <- 100

for (i in seq_along(a_x)) {

  # rearrange first equation (a button) and put into second equation (b button)
  a_presses <- 
  (prize_y[i] - (b_y[i] * prize_x[i] / b_x[i])) / (a_y[i] + (b_y[i] * (a_x[i] / b_x[i]) * -1))

  # calculate b presses
  b_presses <- (prize_x[i] - a_x[i] * a_presses) / b_x[i]

  # deal with floating point precision
  a_presses <- ifelse(abs(a_presses - round(a_presses)) < tolerance, round(a_presses), a_presses)
  b_presses <- ifelse(abs(b_presses - round(b_presses)) < tolerance, round(b_presses), b_presses)

  if (ceiling(a_presses) == a_presses & ceiling(b_presses) == b_presses) {
    n_tokens[i] <- 
    ifelse(a_presses <= max_presses & b_presses <= max_presses, 3 * a_presses + b_presses, NA)
  } else {
    n_tokens[i] <- NA
  }

}

sum(n_tokens, na.rm = TRUE)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

n_tokens <- vector("numeric", length(a_x))

prize_x <- prize_x + 10000000000000
prize_y <- prize_y + 10000000000000

for (i in seq_along(a_x)) {
  
  # rearrange first equation (a button) and put into second equation (b button)
  a_presses <- 
  (prize_y[i] - (b_y[i] * prize_x[i] / b_x[i])) / (a_y[i] + (b_y[i] * (a_x[i] / b_x[i]) * -1))
  
  # calculate b presses
  b_presses <- (prize_x[i] - a_x[i] * a_presses) / b_x[i]
  
  # deal with floating point precision
  a_presses <- ifelse(abs(a_presses - round(a_presses)) < tolerance, round(a_presses), a_presses)
  b_presses <- ifelse(abs(b_presses - round(b_presses)) < tolerance, round(b_presses), b_presses)
  
  if (identical(ceiling(a_presses), a_presses) & identical(ceiling(b_presses), b_presses)) {
    n_tokens[i] <- 3 * a_presses + b_presses
  } else {
    n_tokens[i] <- NA
  }
  
}

sum(n_tokens, na.rm = TRUE)

Sys.time() - start
