library('foreach')
library('ggplot2')
library('animation')
library('reshape')

how_many_neighbors <- function(grid, j, k) {
  size <- nrow(grid)
  count <- 0
  if(j > 1) {
    count <- count + grid[j-1, k]
    if (k > 1) count <- count + grid[j-1, k-1]
    if (k < size) count <- count + grid[j-1,k+1]
  }
  if(j < size) {
    count <- count + grid[j+1, k]
    if (k > 1) count <- count + grid[j+1, k-1]
    if (k < size) count <- count + grid[j, k+1]
  }
  if(k > 1) count <- count + grid[j, k-1]
  if(k < size) count <- count + grid[j, k+1]
  count
}

game_of_life <- function(size = 10, num_reps = 50, prob = c(0.5, 0.5)) {
  grid <- list()
  grid[[1]] <- replicate(size, sample(c(0,1), size, replace = TRUE, prob = prob))
  dev_null <- foreach(i = seq_len(num_reps) + 1) %do% {
    grid[[i]] <- grid[[i-1]]
    foreach(j = seq_len(size)) %:%
      foreach(k = seq_len(size)) %do% {
        num_neighbors <- how_many_neighbors(grid[[i]], j, k)
        alive <- grid[[i]][j,k] == 1
        if(alive && num_neighbors <= 1) grid[[i]][j,k] <- 0
        if(alive && num_neighbors >= 4) grid[[i]][j,k] <- 0
        if(alive && num_neighbors == 3) grid[[i]][j,k] <- 1
      }
  }
  grid
}

grid_to_ggplot <- function(grid) {
  grid <- grid[seq.int(nrow(grid), 1), ]
  grid <- melt(grid)
  grid$value <- factor(ifelse(grid$value, "Alive", "Dead"))
  p <- ggplot(grid, aes(x=X1, y=X2, z = value, color = value))
  p <- p + geom_tile(aes(fill = value))
  p + scale_fill_manual(values = c("Dead" = "White", "Alive" = "black"))
}

game_grids <- game_of_life(size = 20, num_reps = 50, prob = c(0.1, 0.9))
grid_ggplot <- lapply(game_grids, grid_to_ggplot)
grid_ggplot
saveGIF(lapply(grid_ggplot, print), clean = TRUE)
                      