day3 <- readLines("./inputs/03_input.txt")

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

# extract valid mul(X,Y) commands

# what I get here is a vector of the form "X,Y" "X,Y" "X,Y" ...
# which can then be used to split it into the first/second number

# Today I learned about positive lookbehinds (?<=...) and lookaheads (?=...) :-))))

# There are probably ways to match number 1 and 2 simultaneously and store them in 
# e.g. two lists, but I'm not capable of that

nums <- 
unlist(
  regmatches(
    day3, 
    gregexpr(
      "(?<=mul\\()[0-9]{1,3},[0-9]{1,3}(?=\\))", 
      day3,
      perl = TRUE
    )
  )
)

# extract num1 and num2 and multiply them
sum(sapply(strsplit(nums, ","), function(x) prod(as.numeric(x)) ))
# 182619815

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

# throw out everything between don't() and do() or don't()
# Apparently, part 2 only works if I get rid of the line breaks by collapsing
# (day 3 is a vector of length 7 before)
day3 <- gsub("don\\'t\\(\\).*?(do\\(\\)|$)", "", paste0(day3, collapse = ""))

# now apply same code as before
nums <- 
unlist(
  regmatches(
    day3, 
    gregexpr(
      "(?<=mul\\()[0-9]{1,3},[0-9]{1,3}(?=\\))", 
      day3,
      perl = TRUE
    )
  )
)

sum(sapply(strsplit(nums, ","), function(x) prod(as.numeric(x)) ))
# 80747545

Sys.time() - start