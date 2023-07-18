### Practice using Chess Scraping Package

library(tidyverse)
<<<<<<< HEAD
library(chessR)
lgd <- get_game_data("JaseZiv")
head(lgd)
ids
all_chess_gd <- get_game_data(ids)
all_chess_gd
###
m8 <- m8 %>% mutate(GameTime = row_number())
l <- l %>% select(White,Black,WhiteElo,BlackElo,EndDate, time_class, Username, UserColour, UserResult, UserELO)
l <- l %>% filter(time_class == "blitz")
colnames(m8)[11] <- "rating"
Mid <- mdata
bas <- bas %>% mutate(rating = UserELO)

str(df)

player_df <- function(data, player_name, game_type) {
  result <- data %>%
    filter(White == player_name | Black == player_name) %>%
    filter(time_class == game_type) %>%
    mutate(rating = ifelse(White == player_name, WhiteElo, BlackElo))
  
  return(result)
}
l1 <- player_df(l1,"Fire_Gambit", "blitz" )

cas <- player_df(cas, "NO-REGRET", "blitz")

Jase_df <- player_df(lgd, "JaseZiv", "blitz")

jooker2009_df <- player_df(df, "jooker2009", "blitz")

Farias12334_df <- player_df(df, "Farias12334", "blitz")

twoodman_df <- player_df(df, "twoodman", "blitz")

acedeepu_df <- player_df(df, "acedeepu", "blitz")

stevengalway_df <- player_df(df, "stevengalway", "blitz")

EthanWijaja_df <- player_df(df, "EthanWijaja", "blitz")

twoodwood_df <- player_df(df, "twoodwood", "blitz")

Fire_Gambit_df <- player_df(df, "Fire_Gambit", "blitz")

ggplot(Fire_Gambit_df, (aes(x=EndDate, y=rating, color=Username))) + geom_line()

## combining/cleaning

df_all <- rbind(xlabs1_df, Stefan_2003_df, jooker2009_df, Farias12334_df, twoodman_df, acedeepu_df, stevengalway_df, EthanWijaja_df, twoodwood_df, Fire_Gambit_df)
str(df_all)
sub_high$rating <- as.numeric(sub_high$rating)

l1$rating <- as.numeric(l1$rating)
str(Mid)
EthanWijaja_df$rating <- as.numeric(EthanWijaja_df$rating)

convert_to_numeric <- function(data) {
  data$rating <- as.numeric(data$rating)
  return(data)
}

twoodwood_df <- convert_to_numeric(twoodwood_df)

str(twoodwood_df)

df1 <- df_all %>% group_by(EndDate, Username) %>% summarize(rating = round(mean(rating))) 

FG <- Fire_Gambit_df %>% group_by(EndDate, Username) %>% summarize(rating = round(mean(rating))) 

summarized_player_df <- function(data){ 
  data %>% 
    group_by(EndDate, Username) %>% 
    summarize(rating = round(mean(rating))) 
}

XL <- summarized_player_df(xlabs1_df)

str(df1)

## plotting

ggplot(df_all, (aes(x=EndDate, y=rating, color=Username))) + geom_line()

ggplot(df1, (aes(x=EndDate, y=rating, color=Username))) + geom_line()


ggplot(FG, (aes(x=EndDate, y=rating))) + geom_line() + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggplot(df1, (aes(x=EndDate, y=rating, color = Username))) + geom_line() + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df1, (aes(x=lub_month, y=rating, color=Username))) + geom_line() ## + 
  ##scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  ##theme(axis.text.x = element_text(angle = 45, hjust = 1))

# dates
str(all_chess_gd)
library(lubridate)

lubridate::month(df1$EndDate)
lubridate::month(FG$EndDate)

FG$relative_day <- as.numeric(difftime(as.Date(FG$EndDate), min(as.Date(FG$EndDate)), units = "days"))

EW$relative_day <- as.numeric(difftime(as.Date(EW$EndDate), min(as.Date(EW$EndDate)), units = "days"))

XL$relative_day <- as.numeric(difftime(as.Date(XL$EndDate), min(as.Date(XL$EndDate)), units = "days"))

ggplot(FG, (aes(x=relative_day, y=rating, color=Username))) + geom_line()

ggplot(EW, (aes(x=relative_day, y=rating))) + geom_line()

summarized_players <- rbind(EW, FG, AD, FA, JO, SG, ST, TM, TW, XL)

ggplot(summarized_players, (aes(x=relative_day, y=rating, color=Username))) + 
  geom_line() +
  theme_minimal()

#finding average play 

FG <- FG[order(FG$EndDate), ]

date_runs <- rle(as.numeric(FG$EndDate) - min(as.numeric(FG$EndDate)))

consecutive_lengths <- date_runs$lengths

print(consecutive_lengths)
=======


### Function to scrape one player's data:


# function to extract lichess game data
lgd <- get_game_data("Lorenzo1")
lgd
colnames(lgd)
>>>>>>> b2bf308383d038199d51a93a87f65a0757dafc4d
