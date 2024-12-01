day1 <- read.table("./inputs/01_input.txt", header = FALSE)

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

sum(abs(sort(day1$V1) - sort(day1$V2)))

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

# stupid loop :-)

start <- Sys.time()

counter <- 0

for (i in seq_along(day1$V1)) {

  counter <- counter + day1$V1[i] * sum(day1$V1[i] == day1$V2)

}

counter

Sys.time() - start

## PART 2 - V2 ---------------------------------------------------------------------------

# bUt YoU dOn'T nEeD lOoPs In R!

start <- Sys.time()

num_table <- table(day1$V2)[as.character(day1$V1)]

sum(as.numeric(names(num_table)) * num_table, na.rm = TRUE)

Sys.time() - start
