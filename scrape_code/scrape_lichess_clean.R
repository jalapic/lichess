

## Install chessR package

# # install.packages("devtools")
# devtools::install_github("JaseZiv/chessR")

library(chessR)
library(rvest) # needed for scraping teamnames 

# Note, without API token, we can only retrieve 15 games per second from lichess.


## If we just want to get one player's data by username we do the following
# this player only has 35 games so will be quick to get games
lgd <- get_raw_lichess("JaseZiv")

# the 18th column are game moves. This is messy, so only keep first 17
lgd[,1:17]


# Note: any way to do this but to not collect the game move information?



### We need to grab usernames from lichess

# one way is to go to the 'team' pages they have and scrape members
# however these pages don't dynamically load all names - this only grabs approx 30 names
# one R solution around this is using RSelenium (haven't tried yet)

url <- "https://lichess.org/team/im-eric-rosen-fan-club/members"
webpage <- read_html(url)
player_names <- html_nodes(webpage, "td") %>% html_text()
ids <- player_names[which(seq_along(player_names) %% 2 == 1)]
ids


## Another way to do this is to select from usernames from players already in our database
# (also see scraping chess.com R file)




### Loop to collect all games from several players at once
# however, sometimes we get errors for certain players which stops the loop
# unknown why these errors occur
# so probably need to put loop into a try() function so it skips those with errors

# note: only run this if prepare to wait a while to get the games.

res <- vector('list',length(ids))
ids <- 
  for(i in 1:length(ids)){
    res[[i]] <- get_raw_lichess(ids[i])
  }
res

lapply(res, nrow)

### To save output:
# saveRDS(res, "rdata/name_of_file.RData")


