
### Can get data from chess.com also:


library(chessR)
library(tidyverse)
cas <- get_game_data("NO-REGRET")
bas <- get_game_data("jkidjr22")
m1 <- get_game_data("Noctursa")
m2 <- get_game_data("Martin_Stahl")
l1 <- get_game_data("Fire_Gambit")
l2 <- get_game_data("twoodwood")
h8 <- get_game_data("XuYinglun")
m8 <- get_game_data("plax1967")
### How to get more usernames ?  
# could start with opponents of players already got...
# could select by rating, or randomize
# but would need to ensure got players with rating range and game type ranges
# e.g.

unique(c(m2$White,m2$Black))

unique(cas$time_class)

idm <- sample(unique(c(l1$White,l1$Black)),10,F)
idm
l <- get_game_data(idm)
unique(ldata4$Username)
unique(ldata$Username)
# do for these ids - can put in as vector, or do as loop, which is better?
# this takes a long time ! (15 mins for 10 players)
# also, get some warning messages - some NAs added

mdata <- get_game_data(ids)

cdata #64625 rows of data.

head(cdata)

cdata[-c(10,13,23,24)] # removing annoying columns that are too long 

table(rle(cdata$UserResult)$lengths)

## How quick is this compared to lichess?


## Save Data

saveRDS(cdata[-c(10,13,23,24)], "rdata/chesscom.RData")  #by removing these cols for 10 players, halves the memory required.
