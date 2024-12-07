# day7 <- readLines("./inputs/07_input_exp.txt")
day7 <- readLines("./inputs/07_input.txt")

options(scipen = 999)

results <- as.numeric((sapply(strsplit(day7, ":"), `[[`, 1)))
numbers <- lapply(lapply(strsplit(day7, ":"), `[[`, 2), function(x) as.numeric(unlist(strsplit(trimws(x), " "))))

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

valid_sequences <- vector(length = length(results))

for (i in seq_along(numbers)) {
  
  temp_numbers <- numbers[[i]]
  
  temp_results <- c(
    sum(c(temp_numbers[1], temp_numbers[2])),
    prod(c(temp_numbers[1], temp_numbers[2]))
  )
  
  if (length(temp_numbers) > 2) {
    
    for (j in 3:length(temp_numbers)) {
      
      temp_results <- 
      c(temp_results + temp_numbers[j], temp_results * temp_numbers[j])
      
    }
    
  }

  if (results[i] %in% temp_results) valid_sequences[i] <- TRUE 
  
}

sum(results[valid_sequences])

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

valid_sequences <- vector(length = length(results))

for (i in seq_along(numbers)) {
  
  temp_numbers <- numbers[[i]]
  
  temp_results <- c(
    sum(c(temp_numbers[1], temp_numbers[2])),
    prod(c(temp_numbers[1], temp_numbers[2])),
    as.numeric(paste0(temp_numbers[1], temp_numbers[2]))
  )
  
  if (length(temp_numbers) > 2) {
    
    for (j in 3:length(temp_numbers)) {
      
      temp_results <- 
      c(
        temp_results + temp_numbers[j], 
        temp_results * temp_numbers[j], 
        as.numeric(paste0(temp_results, temp_numbers[j]))
      )
      
    }
    
  }

  if (results[i] %in% temp_results) valid_sequences[i] <- TRUE 
  
}

sum(results[valid_sequences])
# 37598910447546

Sys.time() - start
