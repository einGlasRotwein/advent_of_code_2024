# day14 <- readLines("./inputs/14_input_exp.txt")
day14 <- readLines("./inputs/14_input.txt")
width <- 101 # 11 - for example input
height <- 103 # 7

p_xs <- as.numeric(gsub("p\\=(\\d+),.*", "\\1", day14))
p_ys <- as.numeric(gsub("p\\=.*,(\\d+).*v\\=.*", "\\1", day14))
v_xs <- as.numeric(gsub(".*v\\=((-|)[0-9]+),.*", "\\1", day14))
v_ys <- as.numeric(gsub(".*v\\=.*,((-|)[0-9]+).*", "\\1", day14))

## PART 1 --------------------------------------------------------------------------------

start <- Sys.time()

n_steps <- 100

new_xs <- (p_xs + v_xs * n_steps) %% width
new_ys <- (p_ys + v_ys * n_steps) %% height

middle_width <- floor(median(0:width))
middle_height <- floor(median(0:height))

prod(
  sum(new_xs < min(middle_width) & new_ys < min(middle_height)), # Q1
  sum(new_xs > max(middle_width) & new_ys < min(middle_height)), # Q2
  sum(new_xs < min(middle_width) & new_ys > max(middle_height)), # Q3
  sum(new_xs > max(middle_width) & new_ys > max(middle_height)) # Q4
)

Sys.time() - start

## PART 2 --------------------------------------------------------------------------------

start <- Sys.time()

tree_detected <- FALSE
n_seconds <- 1

while(!tree_detected) {
  
  new_xs <- (p_xs + v_xs * n_seconds) %% width
  new_ys <- (p_ys + v_ys * n_seconds) %% height
  
  if (
    any(
      c(
        sum(new_xs < min(middle_width) & new_ys < min(middle_height)), # Q1
        sum(new_xs > max(middle_width) & new_ys < min(middle_height)), # Q2
        sum(new_xs < min(middle_width) & new_ys > max(middle_height)), # Q3
        sum(new_xs > max(middle_width) & new_ys > max(middle_height)) # Q4
      ) >= (length(new_xs) - 200)
    )
  ) tree_detected <- TRUE
  
  n_seconds <- n_seconds + 1
  
}

n_seconds - 1

Sys.time() - start

# print the tree :-)
test_area <- matrix(0, nrow = height, ncol = width)

for (i in seq_along(new_xs)) {
  test_area[cbind(new_ys[i] + 1, new_xs[i] + 1)] <- test_area[cbind(new_ys[i] + 1, new_xs[i] + 1)] + 1
}

# apply(test_area, 1, function(x) paste0(x, collapse = ""))

## PLOT ----------------------------------------------------------------------------------

library(tidyverse)

aoc_green <- "#009900"
github_grey <- "#0d1117"

# AoC-ish plot theme that blends into the GitHub dark mode
aoc_theme <- 
  theme(
    panel.background = element_rect(fill = NA, colour = NA, linewidth = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = github_grey, colour = github_grey),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

day14_plot <- 
data.frame(x = -new_xs, y = -new_ys) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = aoc_green, shape = 3) +
  aoc_theme

save(day14_plot, file = "./plot_data/day14_plot.RData")
