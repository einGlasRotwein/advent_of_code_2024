# day15 <- readLines("./inputs/15_input_exp2.txt")
day15 <- readLines("./inputs/15_input.txt")

source("./code/day15_functions.R")

warehouse <- do.call("rbind", strsplit(day15[1:(which(day15 == "") - 1)], ""))
instructions <- paste0(day15[(which(day15 == "") + 1):length(day15)], collapse = "")
instructions <- unlist(strsplit(instructions, ""))

robot_pos <- which(warehouse == "@", arr.ind = TRUE)

warehouse[warehouse == "@"] <- "."

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

for (i in seq_along(instructions)) {

  temp_direction <- instructions[i]
  after_move <- move_robot(robot_pos, temp_direction, warehouse)
  robot_pos <- after_move[[1]]
  warehouse <- after_move[[2]]

}

boxes <- which(warehouse == "O", arr.ind = TRUE)
boxes <- boxes - 1

sum(boxes[ , 1] * 100 + boxes[ , 2])

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

# will deal with this later :-)))
