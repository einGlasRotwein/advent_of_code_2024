# day9 <- "2333133121414131402"
day9 <- readLines("./inputs/09_input.txt")
day9 <- as.numeric(unlist(strsplit(day9, "")))

options(scipen = 999)

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

# Create file layout
file_blocks <- day9[seq(1, length(day9), 2)]
file_ids <- 0:(length(file_blocks) - 1)
free_space <- day9[seq(2, length(day9), 2)]

rep_files <- 
mapply(function(x, y) {
  
  rep(y, x)
  
}, file_blocks, file_ids)

rep_space <- 
mapply(function(x, y) {
  
  rep(y, x)
  
}, free_space, NA)

# Append free spaces after each file. Keep last file (not followed by free space) for 
# last (rep_files is 1 element longer than rep_space)

rest <- rep_files[[length(rep_files)]]
rep_files <- rep_files[-length(rep_files)]

file_system <- 
mapply(function(x, y) {
  c(x, y)
}, rep_files, rep_space)

file_system <- do.call("c", file_system)
file_system <- c(file_system, rest)

# If we put all files we have directly after each other, our vector is n positions long:
planned_length <- sum(!is.na(file_system))

# I.e., we can fill the NA positions up to:
to_fill <- which(is.na(file_system))[which(is.na(file_system)) < planned_length]

# Fill with files from the end
flat_files <- c(do.call("c", rep_files), rest)

file_system_ordered <- file_system 

file_system_ordered[to_fill] <- 
flat_files[length(flat_files):(length(flat_files) - length(to_fill) + 1)]

file_system_ordered <- file_system_ordered[1:planned_length]

# compute checksum
sum(file_system_ordered * 0:(length(file_system_ordered) - 1))

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

# Meh. Completely different approach with dirty loops.

rep_files <- c(rep_files, list(rest)) # put the rest back for this approach

# convert NAs to -1, because NAs can't be tracked properly with rle()
file_system[is.na(file_system)] <- -1

for (id in rev(file_ids + 1)) {
# for (id in 10:7) {
  
  temp_files <- rep_files[[id]]
  
  # only consider free spaces BEFORE current id
  track_space <- rle(file_system[1:min(which(file_system == unique(temp_files)))])
  free_space <- track_space$lengths[track_space$values == -1]
  
  large_enough <- which(free_space >= length(temp_files))
  
  if (length(large_enough) != 0) {
    
    marker <- min(large_enough)
    
    to_fill <- free_space[1:marker]
    fill_with <- rep(-1, sum(to_fill))
    
    fill_with[(length(fill_with) - free_space[marker] + 1):(length(fill_with) - (free_space[marker] - length(temp_files)))] <- 
    temp_files
    
    file_system[file_system == unique(temp_files)] <- -1
    file_system[file_system == -1][1:length(fill_with)] <- fill_with
    
    track_space <- rle(file_system)
    free_space <- track_space$lengths[track_space$values == -1]
    
  }
  
}

file_system[file_system == -1] <- NA
sum(file_system * 0:(length(file_system) - 1), na.rm = TRUE)

Sys.time() - start
