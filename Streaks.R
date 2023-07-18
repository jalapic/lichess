 head(ldf)
table(ldf$Username) 
unique(ldf$Username)
table(ldf$UserResult)

dd <- ldf[ldf$Username=="dburns2757",]
dd$UserResult
table(dd$UserResult)

x  <-  rle(dd$UserResult)

dd1 <- data.frame(values=x$values, len = x$lengths)
dd1
dd2 <- dd1[dd1$values=="Win",]

head(dd2)
str(dd2)

ggplot(dd2, aes(x=len)) +
  geom_histogram(bins=15,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:15) +
  ggtitle("Win Streak")


ids <- unique(ldf$Username) 
ids

results<-NULL

for(i in 1:length(ids)){
dd <- ldf[ldf$Username==ids[i],]
x  <-  rle(dd$UserResult)
dd1 <- data.frame(values=x$values, len = x$lengths)
results[[i]]<- dd1[dd1$values=="Win",]
}


res <- Map(cbind, results, id = ids)

dx <- data.table::rbindlist(res)
dx
range(dx$len)
dx[dx$len>10,]

ggplot(dx, aes(x=len)) +
  geom_histogram(bins=25,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:20) +
  ggtitle("Win Streaks")

ggplot(dx, aes(x=len)) +
  geom_histogram(bins=25,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:20) +
  facet_wrap(~id) +
  ggtitle("Win Streaks")

#loss
dl <- ldf[ldf$Username=="dburns2757",]
dl$UserResult
table(dl$UserResult)

xl  <-  rle(dl$UserResult)

dl1 <- data.frame(values=xl$values, len = xl$lengths)
dl1
dl2 <- dd1[dd1$values=="Loss",]

head(dl2)
str(dl2)

ggplot(dl2, aes(x=len)) +
  geom_histogram(bins=15,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:15) +
  ggtitle("Loss Streak")


idsl <- unique(ldf$Username) 
idsl

results<-NULL

for(i in 1:length(idsl)){
  dl <- ldf[ldf$Username==idsl[i],]
  xl  <-  rle(dd$UserResult)
  dd1 <- data.frame(values=xl$values, len = xl$lengths)
  results[[i]]<- dd1[dd1$values=="Loss",]
}


resl <- Map(cbind, results, idl = idsl)

dxl <- data.table::rbindlist(resl)
dxl
range(dxl$len)
dxl[dxl$len>5,]

ggplot(dxl, aes(x=len)) +
  geom_histogram(bins=25,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:20) +
  ggtitle("Loss Streaks")

ggplot(dxl, aes(x=len)) +
  geom_histogram(bins=25,color='darkseagreen', fill='lightseagreen') +
  theme_classic() +
  scale_x_continuous(breaks=1:20) +
  facet_wrap(~idl) +
  ggtitle("Loss Streaks")


## save data
head(ldf)

saveRDS(ldf,"lorenzo/streaks.RData")

