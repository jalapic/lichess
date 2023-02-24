# Import Data

library(tidyverse)

ldata <- readRDS("rdata/lichess_pilot.Rdata")

# is the focal player white or black?
ldata$w <- ifelse(ldata$Username == ldata$White, 1, 0)

# result column
ldata$result <- ifelse(ldata$w == 1 & ldata$Result == "1-0" | 
                         ldata$w == 0 & ldata$Result == "0-1", 1, 0)

# white/black elo adjustments
ldata$elof <- ifelse(ldata$w == 1, ldata$WhiteElo, ldata$BlackElo)
ldata$elonf <-ifelse(ldata$w == 0, ldata$WhiteElo, ldata$BlackElo)


ldata

table(ldata$Username)
table(ldata$Variant)
table(ldata$TimeControl)


###################


