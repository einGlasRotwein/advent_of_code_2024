day5 <- readLines("./inputs/05_input.txt")

# updates: everything containing ,
updates <- day5[grepl(",", day5)]
updates <- lapply(strsplit(updates, ","), as.numeric)

# ordering rules: anything containing |
ordering <- day5[grepl("\\|", day5)]
ordering <- lapply(strsplit(ordering, "\\|"), as.numeric)

# I think this will be nicer to handle as vectors. Might be wrong, though.
first <- sapply(ordering, `[[`, 1)
second <- sapply(ordering, `[[`, 2)

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

right_order <- 
sapply(updates, function(x) {
  all(match(first, x) < match(second, x), na.rm = TRUE)
})

# get middle number of each list that has the right order
middle_nums <- sapply(updates[right_order], function(x) x[ceiling(length(x) / 2)])

sum(middle_nums)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

incorrect_updates <- updates[!right_order]

reordered <- 
lapply(incorrect_updates, function(x) {
  
  new_order <- c()
  
  # only relevant orderings
  temp_first <- first[first %in% x]
  temp_second <- second[first %in% x]
  
  # first (or next) number is the one that is never second
  # This should only work up until the second-to-last number (which will 
  # only appear as second number)
  while(length(new_order) != (length(x) - 1)) {
    
    new_order <- c(new_order, unique(temp_first[!temp_first %in% temp_second]))
    
    # remove
    temp_second <- temp_second[temp_first != new_order[length(new_order)]]
    temp_first <- temp_first[temp_first != new_order[length(new_order)]]
    
  }

  # append remaining number
  new_order <- c(new_order, x[!x %in% new_order])

  new_order
  
})

middle_nums <- sapply(reordered, function(x) x[ceiling(length(x) / 2)])

sum(middle_nums)

Sys.time() - start