#  do people who win more on black tend to have a higher rating 
#  than people who win more on white
library(stringr)
library(lubridate)
library(gridExtra)
install.packages("entropy")

df2 <- df_all %>% group_by(UserColour) %>% mutate(win_percent = )

df_all %>% group_by(UserColour) %>% count(UserResult)

df_all %>% group_by(UserColour, Username) %>% count(UserResult)
a1

win_p <- rbind(bwp, wwp)

ggplot(df_all, aes(x = UserColour, y = rating)) +  
  geom_violin() + 
  theme_classic()

ggplot(df_all, aes(x = UserResult, y = rating, fill = UserColour)) +  
  geom_boxplot() + 
  theme_classic()

ggplot(Fire_Gambit_df, aes(x = UserResult, y = rating, fill = UserColour)) +  
  geom_violin() + 
  theme_classic()

ggplot(df_all, aes(x = UserResult, y = rating, color = UserColour)) +  
  geom_violin(position = "identity") + 
  theme_classic()

total <- function(data ,player_name, player_color){data %>% filter(Username == player_name) %>% 
    filter(UserColour == player_color) 

}
total(a1, EthanWijaja, Black )

a1 %>% filter(Username == "EthanWijaja") %>% 
filter(UserColour == "Black")

bwp <- sum(bw$n) / sum(bt$n)
wwp <- sum(ww$n) / sum(wt$n)


# Time to win as influenced by ranking
lubridate::as_datetime(all_chess_gd$StartTime)
str(all_chess_gd)
p <- as.PS
ex <- all_chess_gd

example <- ex[3,20]
practice <- hms(example)
seconds(practice)
x <- seconds(practice)
xnum <- as.numeric(x)


mdata$StartTime <- hms(mdata$StartTime)
mdata$StartTime <- seconds(mdata$StartTime)
mdata$StartTime <- as.numeric(mdata$StartTime)
mdata$EndTime <- hms(mdata$EndTime)
mdata$EndTime <- seconds(mdata$EndTime)
mdata$EndTime <- as.numeric(mdata$EndTime)

mdata$GameTime <- (mdata$EndTime)-(mdata$StartTime)


Low_End <- rbind(l1, l2)
RGT <- rbind(Low_End, Mid, High_End)

Mid <- Mid %>%
filter(!(GameTime < 0))

Mid <- Mid %>%
  filter(!(rating > 1700))

ex <- Fire_Gambit_df

RGT <- RGT %>% mutate(ratings_bin = cut(rating, breaks=c(499, 951, 1199, 1701, 2000, 2500)))

RGT <- RGT %>%
  group_by(ratings_bin) %>%  
  mutate(mean = mean(GameTime),
         upper = mean(GameTime) + 1.96 * sqrt(mean(GameTime)/n()),
         lower = mean(GameTime) - 1.96 * sqrt(mean(GameTime)/n()))

  ggplot(RGT, aes(ratings_bin, mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
  theme_minimal(base_size = 16)
  str(RGT)

CI <- ggplot(RGT, aes(ratings_bin, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  ylim(291, 325)

mean(RGT$GameTime)
sd(RGT$GameTime)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

p1 <- ggplot(Low_End, aes(x=rating, y=GameTime)) + 
  geom_point(alpha = 0.3) +
  stat_smooth(method="lm", se=F) +
  ggtitle("Average Player", subtitle = "r = -0.018        Average Game Time = 298 Seconds") +
  xlab("Rating") + ylab("Game Time")
p2 <- ggplot(Mid, aes(x=rating, y=GameTime)) + 
  geom_point(alpha = 0.3) +
  stat_smooth(method="lm", se=F) +
  ggtitle("High Tier Player", subtitle = "r = 0.085        Average Game Time = 324 Seconds") +
  xlab("Rating") + ylab("Game Time")
p3 <- ggplot(High_End, aes(x=rating, y=GameTime)) + 
  geom_point(alpha = 0.3) +
  stat_smooth(method="lm", se=F) +
  ggtitle("Highest Tier Player", subtitle = "r = -0.05        Average Game Time = 291 Seconds") +
  xlab("Rating") + ylab("Game Time")
p4 <- ggplot(RGT, aes(x=rating, y=GameTime)) + 
  geom_point(alpha = 0.3) +
  stat_smooth(method="lm", se=F) +
  ggtitle("All Players", subtitle = "r = -0.026        Average Game Time = 299 Seconds") +
  xlab("Rating") + ylab("Game Time")

# plots
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
CI

cor(df_all$rating, df_all$GameTime)
cor.test(RGT$rating, RGT$GameTime)
  mean(Low_End$GameTime)
#linear regression investigating the relationship between GameTime and rating
gamerank <- lm(data=RGT, GameTime~rating)
gamerankempty <- lm(data = RGT, GameTime~1)
anova(gamerank, gamerankempty)

#  Cold Streak / Break Analyses
ex <- Low_End
str(l2)

l <- l %>% group_by(Username) %>% mutate(num_games = row_number())

ex <- ex %>% group_by(Username) %>% mutate(relative_day = as.numeric(difftime(as.Date(ex$EndDate), min(as.Date(ex$EndDate)), units = "days")))
?group_by
bas <- bas %>% mutate(num_games = row_number())

ex$relative_day <- as.numeric(difftime(as.Date(ex$EndDate), min(as.Date(ex$EndDate)), units = "days"))

l2$relative_day <- as.numeric(difftime(as.Date(l2$EndDate), min(as.Date(l2$EndDate)), units = "days"))

diffs <- diff(l2$EndDate)
gap_indices <- which(diffs > 6)
gaps <- l2$Username[gap_indices]
daps <- l2$num_games[gap_indices]
print(gaps)
print(daps)
print(gap_indices)
print(l2$rating[gap_indices])
l2$rating[gap_indices+1] - l2$rating[gap_indices]


cas$relative_day <- as.numeric(difftime(as.Date(cas$EndDate), min(as.Date(cas$EndDate)), units = "days"))

diffs <- diff(RGT$EndDate)
gap_indices <- which(diffs > 30)
gaps <- RGT$Username[gap_indices]
daps <- RGT$num_games[gap_indices]
print(gaps)
print(daps)
print(gap_indices)
print(Mid$rating[gap_indices])
Mid$rating[gap_indices+1] - Mid$rating[gap_indices]
str(RGT)
ggplot(subset_l2, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5))

s1 <- ggplot(sub_low, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("Average Player") +
  ylab("Rating") + xlab("# of Games")

s2 <- ggplot(sub_high, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("High Tier Player") +
  ylab("Rating") + xlab("# of Games")

s3 <- ggplot(sub_mid, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("Highest Tier Player") +
  ylab("Rating") + xlab("# of Games")


s30b <- RGT[81948:82007, ]
row.names(s30b) <- NULL
s30b <- s30b %>% mutate(num_games = row_number())

subset_h2 <- subset_h2 %>% select(White,Black,WhiteElo,BlackElo,EndDate, time_class, Username, UserColour, UserResult, rating, num_games)
subset_h2 <- subset_h2 %>% mutate(GameTime = row_number())
sub_mid <- rbind(subset_m1, subset_m2,subset_m3,subset_m4,subset_m5,subset_m6,subset_m7,subset_m8)

breaks_month <- rbind(s30a,s30b,s30c,s30d,s30e,s30f,s30g,s30h)
remove(breaks_five)
#plot
ggplot(subset_l2, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5))

grid.arrange(s1, s2, s3, ncol = 3, nrow = 1)

#group by diff break times
b1 <- ggplot(breaks_one, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("One Day Break") +
  ylab("Rating") + xlab("# of Games")

b2 <- ggplot(breaks_seven, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("One Week Break") +
  ylab("Rating") + xlab("# of Games")

b3 <- ggplot(breaks_month, (aes(x=num_games, y=rating, color = Username))) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = 30.5, lwd=0.5) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  ggtitle("One Month or More Break") +
  ylab("Rating") + xlab("# of Games")

grid.arrange(b1, b2, b3, ncol = 3, nrow = 1)

# E
entropy::entropy.Dirichlet(l1$rating, l1$num_games)
