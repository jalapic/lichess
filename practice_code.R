### Practice using Chess Scraping Package

library(tidyverse)


### Function to scrape one player's data:


# function to extract lichess game data
lgd <- get_game_data("Lorenzo1")
lgd
colnames(lgd)
