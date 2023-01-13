
### Can get data from chess.com also:


library(chessR)
library(tidyverse)

cas <- get_game_data("JaseZiv")
cas


### How to get more usernames ?  
# could start with opponents of players already got...
# could select by rating, or randomize
# but would need to ensure got players with rating range and game type ranges
# e.g.

unique(c(cas$White,cas$Black))

ids <- sample(unique(c(cas$White,cas$Black)),10,F)

# do for these ids - can put in as vector, or do as loop, which is better?
# this takes a long time ! (15 mins for 10 players)
# also, get some warning messages - some NAs added

cdata <- get_game_data(ids)

cdata #64625 rows of data.

head(cdata)

cdata[-c(10,13,23,24)] # removing annoying columns that are too long 

table(rle(cdata$UserResult)$lengths)

## How quick is this compared to lichess?


## Save Data

saveRDS(cdata[-c(10,13,23,24)], "rdata/chesscom.RData")  #by removing these cols for 10 players, halves the memory required.
