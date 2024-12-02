day2 <- readLines("./inputs/02_input.txt")
day2 <- strsplit(day2, " ")
day2 <- lapply(day2, as.numeric)

is_list_ok <- function(x) {
  differences <- diff(x)
  sign_ok <- all(differences < 0) | all(differences > 0)
  steps_ok <- all(abs(differences) >= 1 & abs(differences) <= 3)
  return(sign_ok & steps_ok)
}

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

safe_lists <- sapply(day2, is_list_ok)

sum(safe_lists)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

safe_lists <- 
sapply(day2, function(x) {
  list_ok <- is_list_ok(x)

  for (i in seq_along(x)) {
    list_ok <- list_ok | is_list_ok(x[-i])
    if(list_ok) break
  }

  list_ok
})

sum(safe_lists)

Sys.time() - start
