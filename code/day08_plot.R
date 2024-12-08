# day8 <- readLines("./inputs/08_input_exp.txt")
day8 <- readLines("./inputs/08_input.txt")
day8 <- do.call("rbind", strsplit(day8, ""))

get_antinodes2 <- function(area, a1, a2) {
  
  x_diff <- a1[1] - a2[1]
  y_diff <- a1[2] - a2[2]
  
  output <- list()
  
  n1 <- c(a1[1] + x_diff, a1[2] + y_diff)
  n1_within_bounds <- all(n1 > 0) & n1[1] <= nrow(area) & n1[2] <= ncol(area)
  
  if (n1_within_bounds) {
    output <- c(output, list(n1))
  }
  
  while(n1_within_bounds) {
    n1 <- c(n1[1] + x_diff, n1[2] + y_diff)
    n1_within_bounds <- all(n1 > 0) & n1[1] <= nrow(area) & n1[2] <= ncol(area)
    
    if (n1_within_bounds) {
      output <- c(output, list(n1))
    }
  }
  
  n2 <- c(a2[1] - x_diff, a2[2] - y_diff)
  n2_within_bounds <- all(n2 > 0) & n2[1] <= nrow(area) & n2[2] <= ncol(area)
  
  if (n2_within_bounds) {
    output <- c(output, list(n2))
  }
  
  while(n2_within_bounds) {
    n2 <- c(n2[1] - x_diff, n2[2] - y_diff)
    n2_within_bounds <- all(n2 > 0) & n2[1] <= nrow(area) & n2[2] <= ncol(area)
    
    if (n2_within_bounds) {
      output <- c(output, list(n2))
    }
  }
  
  output <- c(list(a1), list(a2), output)
  
  return(output)
  
}

## PART 2 --------------------------------------------------------------------------------

frequencies <- unique(as.vector(day8))
frequencies <- frequencies[frequencies != "."]

antinodes <- list()
freqs <- vector("character")
pair_ids <- vector("character")

for (frq in frequencies) {
  
  antennas <- which(day8 == frq, arr.ind = TRUE)
  antenna_pairs <- combn(nrow(antennas), 2)
  n_freqs <- 0
  
  for (j_pair in 1:ncol(antenna_pairs)) {
    
    a1 <- antennas[antenna_pairs[1, j_pair], ]
    a2 <- antennas[antenna_pairs[2, j_pair], ]
    
    antinodes <- c(antinodes, get_antinodes2(day8, a1, a2))
    n_freqs <- n_freqs + length(get_antinodes2(day8, a1, a2))
    pair_ids <- 
      c(
        pair_ids, 
        rep(
          paste0(antenna_pairs[1, j_pair], "-", antenna_pairs[2, j_pair]),
          length(get_antinodes2(day8, a1, a2))
        )
      )
    
  }
  
  freqs <- c(freqs, rep(frq, n_freqs))
  
}

length(unique(antinodes))

## PLOT ----------------------------------------------------------------------------------

library(tidyverse)
library(extrafont)

aoc_green <- "#009900"
aoc_gold <- "#ffff66"
github_grey <- "#0d1117"

# AoC-ish plot theme that blends into the GitHub dark mode
aoc_theme <- 
  theme(
    panel.background = element_rect(fill = NA, colour = NA, linewidth = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = github_grey, colour = github_grey),
    plot.title = element_text(colour = "white", size = 20, family = "Consolas"),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = 16, colour = aoc_green),
    axis.text = element_text(size = 12, colour = "white", family = "Consolas"),
    axis.title = element_text(size = 14, colour = "white", family = "Consolas"),
    axis.line = element_line(colour = "white", linewidth = .5),
    axis.ticks = element_line(colour = "white", linewidth = .5),
    legend.position = "none"
  )


antinodes <- as.data.frame(do.call("rbind", antinodes))
antinodes$freq <- freqs
antinodes$id <- pair_ids

antennas <- which(antennas <- day8 != ".", arr.ind = TRUE)

antenna_names <- 
  apply(antennas, 1, function(x) {
    day8[x[1], x[2]]
  })

antennas <- as.data.frame(antennas)
antennas$freq <- antenna_names

day8_plot <- 
  antinodes %>% 
  arrange(freq, id, row, col) %>% 
  ggplot(aes(x = row, y = col, colour = freq)) +
  geom_path(aes(group = interaction(freq, id)), alpha = .3) +
  geom_point(alpha = .4) +
  geom_point(data = antennas, size = 3) +
  theme_classic() +
  theme(legend.position = "none") +
  aoc_theme

save(day8_plot, file = "day8_plot.RData")
