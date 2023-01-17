ldata <- readRDS("rdata/lichess_pilot.Rdata")
str(ldata)

head(ldata)
library(tidyverse)

df <- rbind(ldata %>% select(ID = White, Rating = WhiteElo), 
      ldata %>% select(ID = Black, Rating = BlackElo)) %>%
  group_by(ID) %>%
  summarize(Rating = mean(as.numeric(Rating), na.rm = T))

ggplot(df, aes(x=Rating)) +
  geom_histogram()

df %>%
  filter(Rating < 1200)


cdata <- readRDS("rdata/chesscom.Rdata")

# pick a few players, split by time control, make a model that predicts future outcomes
# by previous ones (look at monkey model)
  # filter out bots
  # use current rating in the model


cdata %>%
  filter(White == "olivierjarny") %>%
  mutate(gameID = row_number()) %>%
  ggplot(.,(aes(x=gameID, y=as.numeric(WhiteElo)))) +
  geom_line()

