day4 <- read.table("./inputs/04_input.txt")
# day4 <- read.table("./inputs/04_input_exp.txt")
day4 <- do.call("rbind", strsplit(day4$V1, ""))

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

matches_if_valid <- function(m, pos, to_find) {
  is_invalid <- any(pos < 0) | any(pos[ , 1] > nrow(m)) | any(pos[ , 2] > ncol(m))

  return(ifelse(!is_invalid, paste0(m[pos], collapse = ""), "0") == to_find)
}

check_string_around <- function(m, x, y, to_find) {

  n_matches <- 0
  
  n_letters <- nchar(to_find)

  # right
  temp_pos <- cbind(x, y:(y + n_letters - 1))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # left
  temp_pos <- cbind(x, y:(y - (n_letters - 1)))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # up
  temp_pos <- cbind(x:(x - (n_letters - 1)), y)
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # down
  temp_pos <- cbind(x:(x + n_letters - 1), y)
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # UR
  temp_pos <- cbind(x:(x - (n_letters - 1)), y:(y + n_letters - 1))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # DR
  temp_pos <- cbind(x:(x + n_letters - 1), y:(y + n_letters - 1))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # UL
  temp_pos <- cbind(x:(x - (n_letters - 1)), y:(y - (n_letters - 1)))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)
  
  # DL
  temp_pos <- cbind(x:(x + n_letters - 1), y:(y - (n_letters - 1)))
  n_matches <- n_matches + matches_if_valid(m, temp_pos, to_find)

  return(n_matches)
  
}

x_pos <- which(day4 == "X", arr.ind = TRUE)
xmas_counter <- 0

for (i in 1:nrow(x_pos)) {
  xmas_counter <- xmas_counter + check_string_around(day4, x_pos[i, 1], x_pos[i, 2], "XMAS")
}

xmas_counter

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

get_if_valid <- function(m, pos) {
  is_invalid <- any(pos <= 0) | any(pos[ , 1] > nrow(m)) | any(pos[ , 2] > ncol(m))

  return(ifelse(!is_invalid, paste0(m[pos], collapse = ""), "0"))
}

check_string_diag <- function(m, x, y, n_letters, letter_set) {

  # UR
  temp_pos <- cbind(x - n_letters, y + n_letters)
  ur <- get_if_valid(m, temp_pos)
  
  # DR
  temp_pos <- cbind(x + n_letters, y + n_letters)
  dr <- get_if_valid(m, temp_pos)
  
  # UL
  temp_pos <- cbind(x - n_letters, y - n_letters)
  ul <- get_if_valid(m, temp_pos)
  
  # DL
  temp_pos <- cbind(x + n_letters, y - n_letters)
  dl <- get_if_valid(m, temp_pos)

  is_valid <- all(letter_set %in% c(ul, dr)) & all(letter_set %in% c(ur, dl))

  return(is_valid)
  
}

a_pos <- which(day4 == "A", arr.ind = TRUE)
xmas_counter <- 0

for (i in 1:nrow(a_pos)) {
  xmas_counter <- xmas_counter + check_string_diag(day4, a_pos[i, 1], a_pos[i, 2], 1, c("M", "S"))
}

xmas_counter

Sys.time() - start
