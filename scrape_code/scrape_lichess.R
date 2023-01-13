
### Any way to do this but to not collect the game move information - it takes too long to grab.

# install.packages("devtools")
devtools::install_github("JaseZiv/chessR")
library(chessR)

# function to extract chess.com game data
chessdotcom_game_data_all_months <- get_raw_chessdotcom(usernames = "JaseZiv")
chessdotcom_game_data_all_months


chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
chessdotcom_hikaru_recent

# function to extract lichess game data
lichess_game_data <- get_raw_lichess("Georges")
lichess_game_data

chess_analysis_single <- get_game_data("JaseZiv")
chess_analysis_single


chess_analysis_multiple <- get_game_data(c("JaseZiv", "Smudgy1"))
chess_analysis_multiple


lichess_game_data <- get_raw_lichess("JaseZiv")
lichess_game_data[,1:17]

lichess_game_data <- get_raw_lichess("penguingim1")
lichess_game_data[,1:17]

x <- ifelse(lichess_game_data[,1:17]$White=="penguingim1", lichess_game_data[,1:17]$WhiteElo, lichess_game_data[,1:17]$BlackElo)
plot(x,type="l")
getwd()
saveRDS(lichess_game_data[,1:17], "lichess.RData")


p1<-lichess_game_data[,1:17]
head(p1)
table(p1$TimeControl)

library(tidyverse)
p1[p1$TimeControl=="0+1",]




l <- get_raw_lichess("ggploot")
l[,1:17]
table(l$TimeControl)
nrow(l)

l2 <- get_raw_lichess("Gemontytus")
l2[,1:17]
nrow(l2)
table(l2$TimeControl)

l3 <- get_raw_lichess("Tzarwyn")
l3[,1:17]
nrow(l3)
table(l3$TimeControl)

l4 <- get_raw_lichess("taliahelal")
l4[,1:17]
nrow(l4)
table(l4$TimeControl)

l5 <- get_raw_lichess("GGdelajedrez")
l5[,1:17]
nrow(l5)
table(l5$TimeControl)

l6 <- get_raw_lichess("Esellaran")
nrow(l6)
table(l6$TimeControl)

l7 <- get_raw_lichess("serg333")
nrow(l7)
table(l7$TimeControl)

l8 <- get_raw_lichess("VictorPenades")
nrow(l8)
table(l8$TimeControl)

l9 <- get_raw_lichess("JimGon")
nrow(l9)
table(l9$TimeControl)

l10 <- get_raw_lichess("QueenRosieMary")
nrow(l10)
table(l10$TimeControl)

# do a bunch

# Page doesn't dynamically load all names - this only grabs approx 26 names
library(rvest)
url <- "https://lichess.org/team/im-eric-rosen-fan-club/members"
webpage <- read_html(url)
player_names <- html_nodes(webpage, "td") %>% html_text()
print(player_names)

ids <- player_names[which(seq_along(player_names) %% 2 == 1)]
ids

# We could try to use RSelenium to get it to scroll while collecting.

##########################

# if do this - might need to do try()
# as got this error on 19th out of 30 players:

res <- vector('list',length(ids))
ids <- 
for(i in 1:length(ids)){
  res[[i]] <- get_raw_lichess(ids[i])
}
res

lapply(res, nrow)
res[[14]][1:6]



url <- "https://lichess.org/team/--elite-chess-players-union--/members"
webpage <- read_html(url)
player_names <- html_nodes(webpage, "td") %>% html_text()
ids <- player_names[which(seq_along(player_names) %% 2 == 1)]
ids

# We could try to use RSelenium to get it to scroll while collecting.

##########################

# if do this - might need to do try()
# as got this error on 4th out of 30 players:

res1 <- vector('list',length(ids))
ids <- 
  for(i in 1:length(ids)){
    res1[[i]] <- get_raw_lichess(ids[i])
  }
res1

lapply(res1, nrow)
res1[[14]][1:6]

res2 <- vector('list',length(ids))
ids <- 
  for(i in 6:length(ids)){
    res2[[i]] <- get_raw_lichess(ids[i])
  }
res2

lapply(res2, nrow)
res2[[14]][1:6]


getwd()
setwd("C:/Users/curle/Documents/lichess")
bigl<-list(p1,l,l2,l3,l4,l5,l6,l7,l8,l9,l10,res,res1,res2)
saveRDS(bigl, "lichess.RData")
